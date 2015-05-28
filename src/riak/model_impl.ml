open Prob_cache_common
module OldList = List
module OldSequence = Sequence
open Core.Std
module Float = CCFloat (* for pretty printing, ord, etc *)
module CoreList = List
module List = CCList
open Async.Std
module Result = Deferred.Result
module Sequence = OldSequence
module Fun = CCFun

(** Represents a single event- must be protobuf capable, comparable, and pretty printable *)
module type EVENT =
sig
  type t [@@deriving protobuf, show, ord]
  include Events_common.EVENT with type t := t
end

(** Represents an abstract collection of events, must be protobuf capable and pretty printable *)
module type EVENTS =
sig
  type t [@@deriving protobuf, show]
  module Event : EVENT
  include Events_common.EVENTS with module Event := Event and type t := t
end

module Data = Model_intf.Data

(** A module type provided polymorphic probability model caches. Uses in distributed models backed by riak *)
module type S =  Model_intf.S

module Make_for_events (Events:EVENTS) : S with module Events = Events =
struct
  module Events = Events
  module Event = Events.Event
  module Cache = Cache.Make(Events)(Data)
  module Int = CCInt

  type prior_count = Events.t -> int
  type prior_exp = Events.t -> float

  type update_rule = Events.t Update_rules.Update_fn.t

  and t = {
    name : string;
    cache : Cache.t;
    prior_count : prior_count;
    prior_exp : prior_exp; 
    update_rule : update_rule }

  let default_prior_count (e:Events.t) = 0

  let default_prior_exp (e:Events.t) = 0.

  let default_update_rule : update_rule = Update_rules.mean

  let create ?(update_rule=default_update_rule) ?(prior_count=default_prior_count)
    ?(prior_exp=default_prior_exp) cache =
      {cache;prior_count;prior_exp;update_rule;name=Cache.get_bucket cache}

  let _data events t =
   let open Cache.Robj in Cache.get t.cache events >>| function
    | Ok robj ->
        (match robj.contents with
      | [] -> Ok None
      | [d] -> Ok (Some (Content.value d))
      | l -> failwith "should only be one content value")
    | Error `Notfound -> Ok None
    | Error e -> Error e

  let data ?(cond=Events.empty) events t =
   let joined_events = Events.join cond events in
   _data joined_events t

  let count (events:Events.t) t : (int, [> Opts.Get.error]) Result.t =
  let open Cache.Robj in data events t >>| function
    | Ok (Some data) -> Ok (Data.count data)
    | Ok None -> Ok (t.prior_count events)
    | Error e -> Error e

  let descriptive_stat ?cond events prior_fn stat_fn t =
    let open Result.Monad_infix in
    data ?cond events t >>| fun d -> CCOpt.get_lazy (fun () -> prior_fn events) (CCOpt.map stat_fn d)

  let exp ?(cond=Events.empty) (events:Events.t) t =
    descriptive_stat ~cond events t.prior_exp Data.expect t

  let var ?(cond=Events.empty) (events:Events.t) t =
    descriptive_stat ~cond events (Fun.const 0.0) Data.var t

  let max ?(cond=Events.empty) (events:Events.t) t =
    descriptive_stat ~cond events t.prior_exp Data.max t

  let min ?(cond=Events.empty) (events:Events.t) t =
    descriptive_stat ~cond events t.prior_exp Data.min t

  let last ?(cond=Events.empty) (events:Events.t) t =
    descriptive_stat ~cond events t.prior_exp Data.last t

  let sum ?(cond=Events.empty) (events:Events.t) t =
    descriptive_stat ~cond events (Fun.const 0.0) Data.sum t

  let prob ?(cond=Events.empty) (events:Events.t) t : (Core.Std.Float.t, [> Opts.Get.error]) Result.t =
    let open Result.Monad_infix in
    count (Events.join events cond) t >>=
    fun event_count -> count cond t >>=
    fun given_cond_count -> (if given_cond_count = 0 && (not (Events.is_empty cond))
                             then count Events.empty t else Deferred.return (Ok given_cond_count))
    >>| fun cond_count -> if (cond_count = 0) then Float.of_int 0 else
      (Float.of_int event_count) /. (Float.of_int cond_count)


  let update ?(cnt=1) ?(exp=1.0) (events:Events.t) (t:t) =
    let open Result.Monad_infix in
    let open Cache.Robj in data events t >>=
    fun d_opt -> let d = Data.update
      ~cnt ~exp
      ~update_rule:t.update_rule
      ~prior_count:t.prior_count ~prior_exp:t.prior_exp
      events
      d_opt in
    Cache.put t.cache ~k:events (Cache.Robj.of_value d)

  let join_data data (events:Events.t) (t:t) =
    let open Result.Monad_infix in
    _data events t >>= fun data_opt ->
      let data = CCOpt.maybe (fun orig -> Data.join
        ~obs:events ~update_rule:t.update_rule orig data) data data_opt in
        Cache.put t.cache ~k:events (Cache.Robj.of_value data) >>|
        Fun.const t

  let observe ?(cnt=1) ?(exp=1.0) (events:Events.t) t =
    let open Result.Monad_infix in
    let subsets = Events.subsets events in
    List.fold_right
    (fun e d -> Result.ignore @@
    Result.bind d (Fun.const @@ update ~cnt ~exp e t))
    subsets (Result.return ())
    >>| Fun.const t

  let observe_data data events t =
    let open Result.Monad_infix in
    let subsets = Events.subsets events in
    List.fold_right
    (fun e d -> Result.ignore @@
    Result.bind d (Fun.const @@ join_data data e t))
    subsets (Result.return ())
    >>| Fun.const t

  let with_model ?update_rule ?prior_count
    ?prior_exp ~host ~port ~(name:string) f =
      let open Result.Monad_infix in
      Cache.with_cache ~host ~port ~bucket:name (fun c ->
        let (m:t) = create ?prior_count ?update_rule ?prior_exp c in f m)

  let name t = t.name
end

module Make_event_set(Event:EVENT) : EVENTS with module Event = Event = 
struct
  module Event = Event
  module Hashset = Containers_misc.Hashset
  type t = Event.t Hashset.t

  let to_list t = List.sort_uniq ~cmp:Event.compare (Sequence.to_list (Hashset.to_seq t))
  let of_list l =
    let h = Hashset.empty (CoreList.length l) in
    CoreList.iter ~f:(fun e -> Hashset.add h e) l;h

  module List = OldList
  type event_list = Event.t list [@@deriving protobuf]

  let join (t:t) (t':t) : t = Hashset.union t t'
  let empty = of_list []
  let is_empty t = Hashset.cardinal t = 0

  let subsets (t:t) : t list =
    List.map of_list (Powerset.generate (to_list t))

  let to_protobuf t e = event_list_to_protobuf (to_list t) e
  let from_protobuf d = of_list (event_list_from_protobuf d)

let pp (f:Format.formatter) t = Hashset.iter (Event.pp f) t
let show t =  Hashset.fold (fun acc e -> (Event.show e) ^ ";" ^ acc) "" t

end

module Make_event_sequence(Event:EVENT) : EVENTS with module Event = Event =
struct
  module Event = Event
  module List = OldList
  type t = Event.t list [@@deriving protobuf, show]
  let of_list l = l
  let to_list l = l
  let join = CCList.append
  let empty = CCList.empty
  let subsets (l:t) = let (accum, _) =
    List.fold_left
      (fun ((accum: t list), (l:t)) e -> let l' = l@[e] in (l'::accum, l'))
      ([], [])
      l
  in
  []::accum
  let is_empty t = empty = t
end
