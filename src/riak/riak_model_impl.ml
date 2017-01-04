open Prob_cache.Std
open Or_errors.Std
open Or_errors_async.Std
module OldList = List
module OldSequence = Sequence
module Float = CCFloat (* for pretty printing, ord, etc *)
module CoreList = Core.Std.List
module List = CCList
module Hashset = Prob_cache_hashset
open Async.Std
module Result = Deferred.Result
module Sequence = OldSequence
module Fun = CCFun

(** Represents a single event- must be protobuf capable, comparable, and pretty printable *)
module type EVENT = Riak_model_intf.EVENT

(** Represents an abstract collection of events, must be protobuf capable and pretty printable *)
module type EVENTS = Riak_model_intf.EVENTS

module Data = Riak_model_intf.Data

(** A module type provided polymorphic probability model caches. Uses in distributed models backed by riak *)
module type S = Riak_model_intf.S

module Make(Events:EVENTS) : S with module Events = Events =
struct
  module Cache = Cache.Make(Events)(Data)
  module Int = Core.Std.Int
  module T =
  struct
    module Events = Events
    module Or_error = Or_error
    module Data = Data
    type prior_count = Events.t -> int
    type prior_exp = Events.t -> float

    type update_rule = Events.t Data.update_rule and t = {
      (*name : string; *)
      cache : Cache.t;
      prior_count : prior_count;
      prior_exp : prior_exp;
      update_rule : update_rule }

    let default_prior_count (e:Events.t) = 0

    let default_prior_exp (e:Events.t) = 0.

    let default_update_rule : update_rule = Update_rules.mean
  end

  module Create_fun : CREATE_FUN with
    type t = T.t and
    module Events = T.Events and
    module Data = T.Data and
    module Or_error = T.Or_error =
  struct
  include T
  let of_cache
      ?(update_rule=default_update_rule)
      ?(prior_count=default_prior_count)
      ?(prior_exp=default_prior_exp) cache = 
        {cache;prior_count;prior_exp;update_rule}
  let name t = (*let open T in *) Cache.get_bucket t.cache
  let update_rule t = t.update_rule

  module Options =
    struct
      module Cache_name = Prob_cache_options.Cache_name
      module Prior =  Prob_cache_prior.Make(Events)(Data)
      module Update = Prob_cache_update_rule_option.Make(Events)(Data)

      class options(name:string)(prior:Prior.t)(update:Update.t)
          (host:string) (port:int) =
      object
          inherit Prob_cache_options.Traits.cache_name(name)
          inherit Prior.prior(prior#prior_exp)(prior#prior_count)
          inherit Update.update(update#update)
          method host = host
          method port = port
      end
      type t = options
      let default_host = "localhost"
      let default_port = 0
      let default = new options
        Cache_name.default#cache_name
        Prior.default
        Update.default
        default_host
        default_port
    end

  exception Connection_error of string

  let with_cache ?(opt=Options.default) (f:t -> 'a Or_error.t) : 'a Or_error.t =
    let open Or_error.Monad_infix in
    let of_cache = of_cache
      ~update_rule:opt#update
      ~prior_count:opt#prior_count
      ~prior_exp:opt#prior_exp in
    Cache.with_cache ~host:opt#host ~bucket:opt#cache_name ~port:opt#port
    (fun c -> Deferred.Result.return @@ f (of_cache c)) |> fun r ->
    Deferred.Result.map_error r
        ~f:(function | `Bad_conn | `Conn_error ->
        Connection_error (Format.sprintf "host/port: %s:%d" opt#host opt#port))
   |> Deferred.Or_error.of_exn_result
   |> Deferred.Or_error.join

  end

  module Data_fun : DATA_FUN with
    type t = T.t and
    module Events = T.Events and
    module Data = T.Data and
    module Or_error = T.Or_error =
  struct
  include T


  exception Data_not_found
  exception Data_assertion of string
  exception Data_exception of exn


  let data events t : Data.t Or_error.t  =
   let open Cache.Robj in Cache.get t.cache events |> fun b ->
       Result.map_error b ~f:(function
          | `Notfound -> Error.of_exn Data_not_found
          | _ -> Error.of_exn (Data_assertion "unchecked riak cache case"))
      |> fun b -> Or_error.of_result b |> fun b -> Or_error.bind b
        (fun robj -> match robj.contents with
     | [] -> Or_error.return Data.empty
      | [d] -> Or_error.return (Content.value d)
      | l ->  Or_error.return (List.fold_right (fun c ->
              Data.join ~obs:events ~update_rule:t.update_rule
                (Content.value c)) l Data.empty))
(*Or_error.fail
          (Error.of_exn (Data_assertion "should only be one content value")) *)
(*
  let data ?(cond=Events.empty) events t =
   let joined_events = Events.join cond events in
   _data joined_events t*)
  end

  module Observe_data_fun : OBSERVE_DATA_FUN with
    type t = T.t and
    module Events = T.Events and
    module Data = T.Data and
    module Or_error = T.Or_error =
  struct
  include Data_fun
(*
  let update ?(cnt=1) ?(exp=1.0) (events:Events.t) (t:t) : Data.t =
    let open Or_error.Monad_infix in
    data events t >>= fun d -> let d' = Data.update
      ~cnt ~exp
      ~update_rule:t.T.update_rule
      ~prior_count:t.T.prior_count
      ~prior_exp:t.T.prior_exp
      events
      (Some d) in
    let v = Cache.Robj.of_value d' in
    let put_result = Cache.put t.T.cache ~k:events v in
    Or_error.return d'
*)
  exception Data_put_error
  let join_data to_join (events:Events.t) (t:t) (*: Data.t * Events.t option Or_error.t *)=
    let open Or_error.Monad_infix in
    data events t >>= fun orig -> Data.join 
      ~obs:events ~update_rule:t.T.update_rule orig to_join |> Cache.Robj.of_value |>
        Cache.put t.T.cache ~k:events (* (Cache.Robj.of_value data') *) |>
        Deferred.Result.map_error ~f:
(function
| `Error
| `Bad_conn
| `Bad_payload
| `Incomplete_payload
| `Overflow
| `Protobuf_encoder_error
| `Unknown_type
| `Wrong_type ->
 (Error.of_exn Data_put_error)) |> Or_error.of_result

   let observe_data data events t : t Or_error.t =
    let subsets = Events.subsets events in
    let iter e d = Or_error.bind d (Fun.const @@ join_data data e t) |>
      Or_error.ignore in
    List.fold_right iter subsets (Or_error.return ()) |>
    Fun.const @@ (Or_error.return t)
  end

  module Fold_fun : FOLD_FUN with
    type t = T.t and
    module Entry.Events = T.Events and
    module Entry.Data = T.Data and
    module Or_error = T.Or_error =
  struct
    include T
    module Entry : Entry.S with
      module Events = Events and module Data = Data =
    struct
      module I = Entry.Make(Events)(Data)
      module Events = Events
      module Data = Data
      include (I : module type of I with
        module Events := Events and
        module Data := Data)
    end
    module Fold : Fold.S with
      module Entry = Entry  =
    struct
      module I = Fold.Make(Entry)
      module Entry = Entry
      include (I : module type of I with
        module Entry := Entry)
      let compare _ = failwith("nyi")
    end
    let fold _ = failwith("nyi")
  end

  module Model_kernel =
  struct
    module K = Model_kernel.Make
    (Events)
    (Create_fun)
    (Data_fun)
    (Observe_data_fun)
    (Fold_fun)
    module Events = Events
    module Data = Data
    include (K:module type of K with module Events := Events and module Data := Data)
  end


  module Decorated = Model_decorator.Make(Model_kernel)
  module Event = Events.Event
  module Data = Data
  include (Decorated :
            module type of Decorated with
            module Events := Events and
            module Event := Event and
            module Data := Data and
            module Or_error := Or_error)
end

module Set = struct
module Make(Event:EVENT) :
  EVENTS with module Event = Event =
    struct
  module EventSet =
    struct
      module Event = Event
      type t = Event.t Hashset.t
  let to_list t = List.sort_uniq ~cmp:Event.compare
    (Sequence.to_list (Hashset.to_seq t))
  let of_list l =
    let h = Hashset.empty (CoreList.length l) in
    CoreList.iter ~f:(fun e -> Hashset.add h e) l;h

  module List = OldList
  type event_list = Event.t list [@@deriving protobuf]

  let join (t:t) (t':t) : t = Hashset.union t t'
  let empty = of_list []
  let is_empty t = Hashset.cardinal t = 0

  let subsets (t:t) : t list =
    List.map of_list (Prob_cache_powerset.generate (to_list t))

  let to_protobuf t e = event_list_to_protobuf (to_list t) e
  let from_protobuf d = of_list (event_list_from_protobuf d)

  let iter = Hashset.iter
  let fold f t acc = Hashset.fold (fun acc e -> f e acc) acc t
  let filter f t = Hashset.filter f t; t
  let remove t e = Hashset.remove t e; t
  let add t e = Hashset.add t e; t
  let pp (f:Format.formatter) t = Hashset.iter (Event.pp f) t
  let show t = Hashset.fold (fun acc e -> (Event.show e) ^ ";" ^ acc) "" t
end
  include EventSet
  include (Events.Make(EventSet) :
    module type of Events.Make(EventSet) with module Event := Event)
end
end

module Sequence =
struct
module Make(Event:EVENT) : EVENTS with module Event = Event =
struct
module Seq =
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
  let iter = List.iter
  let filter = CCList.filter
  let remove t x = CCList.remove ~x t
  let fold = CCList.fold_right
  let add t x = CCList.append t [x]
end
  include Seq
  include (Events.Make(Seq) :
    module type of Events.Make(Seq) with module Event := Event)
end
end
