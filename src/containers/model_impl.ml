open Prob_cache_common
open Model.Std
module Float = CCFloat

(** Represents a single event- must be comparable and showable *)
module type EVENT = Model_intf.EVENT 

(** Represents an abstract collection of events *)
module type EVENTS = Model_intf.EVENTS

(** A module type provided polymorphic probability model caches. 
    Uses in memory models backed by the containers api *)

module type S = Model_intf.S
module type S_KERNEL = Model_intf.S_KERNEL
module Data = Model_intf.Data
module Fun = CCFun
  
module Make_for_events (Events:EVENTS) : S_KERNEL with module Event = Events.Event =
struct
  module Event = Events.Event
  module Events = Events
  module Cache = CCMap.Make(Events)
  module Int = CCInt
  module Data = Data

 type prior_count = Events.t -> int
 type prior_exp = Events.t -> float

  module T = struct 
    type update_rule = Events.t Data.update_rule

  and t = {
    name : string;
    cache : Data.t Cache.t;
    prior_count : prior_count;
    prior_exp : prior_exp;
    update_rule : update_rule }

    let default_prior_count (e:Events.t) = 0

  let default_prior_exp (e:Events.t) = 0.0

  let default_update_rule : update_rule = Update_rules.mean
  end

  module Create_fun : CREATE_FUN = struct
    include T
    let create ?(update_rule=default_update_rule) ?(prior_count=default_prior_count)
    ?(prior_exp=default_prior_exp) ~(name:string) : t = {name;cache=Cache.empty;prior_count;prior_exp;update_rule}
  end
  

  module Observe_fun : OBSERVE_FUN = struct
    include T
    let create ?(update_rule=default_update_rule) ?(prior_count=default_prior_count)
    ?(prior_exp=default_prior_exp) ~(name:string) : t = {name;cache=Cache.empty;prior_count;prior_exp;update_rule}
  end
  
  let count (events:Events.t) (t:t) : int =
    CCOpt.get_lazy (fun () -> t.prior_count events) (CCOpt.map (fun d -> Data.count d) (Cache.get events t.cache))

  let data ?(cond=Events.empty) (events:Events.t) (t:t) : Data.t option =
    let joined_events = Events.join cond events in
    Cache.get joined_events t.cache

  let descriptive_stat ?cond (events:Events.t) t prior_fn stat_fn =
      CCOpt.get_lazy (fun () -> prior_fn events) (CCOpt.map stat_fn (data ?cond events t))
  let prob ?(cond=Events.empty) (events:Events.t) (t:t) =
   let cond_count = count cond t in
    (* (a) If the conditional probability is zero then is so must been the whole probability 
       (b) If the conditional (possibly empty) events have been observed then we normalize by its frequency count 
     *)
    if (cond_count = 0) then (Float.of_int 0) else
    let joined_events_count = count (Events.join cond events) t in
    (Float.of_int joined_events_count) /. (Float.of_int cond_count)

  let _observe ~cnt ~exp (events:Events.t) (t:t) =
    let d = Data.update ~cnt ~exp ~update_rule:t.update_rule ~prior_count:t.prior_count
      ~prior_exp:t.prior_exp events (Cache.get events t.cache) in
    {t with cache=Cache.add events d t.cache}

  let _observe_data data (events:Events.t) (t:t) =
    let orig_opt = Cache.get events t.cache in
    let data = CCOpt.maybe (fun orig ->
      Data.join ~obs:events ~update_rule:t.update_rule orig data) data orig_opt in
    {t with cache=Cache.add events data t.cache}

  let observe ?(cnt=1) ?(exp=1.0) (events:Events.t) (t:t) : t =
    CCList.fold_right (fun l t -> _observe_data (Data.create  ~cnt ~exp) l t) (Events.subsets events) t

  let observe_data data (events:Events.t) (t:t) : t =
    CCList.fold_right (fun l t -> _observe_data data l t) (Events.subsets events) t

  let name t = t.name
end

module Make_event_set(Event:EVENT) : EVENTS with module Event = Event =
struct 
  module Multiset = 
    struct 
       module Event = Event
       include CCMultiSet.Make(Event)
       let show t = to_list t |> List.map Event.show |> String.concat " & "
       let pp formatter t = Format.fprintf formatter "%s" (show t)
       let join = union
       let subsets t = List.map of_list (Powerset.generate (to_list t))
       let filter f l = to_list l |> CCList.filter f |> of_list
       let fold f t acc = to_list t |> fun t -> CCList.fold_right f t acc
       let iter f t = iter t @@ fun _ e -> f e 
     end
  include Multiset
  include (Events_common.Make(Multiset) :
    module type of Events_common.Make(Multiset) with module Event := Event)
end


module Make_event_sequence(Event:EVENT) : EVENTS with module Event = Event =
struct
  module OrdList = 
    struct
      module Event = Event
      type t = Event.t list [@@deriving ord]
      let of_list l = l
      let to_list l = l
      let join = CCList.append
      let empty = CCList.empty
      let subsets (l:t) = let (accum, _) =
        List.fold_left
         (fun ((accum: t list), (l:t)) e -> let l' = l@[e] in (l'::accum, l'))
         ([], []) l in []::accum
     let iter  = List.iter 
     let fold = CCList.fold_right
     let filter = CCList.filter
     let remove (t:t) x = CCList.remove ~x t
     let add t e = join t [e]
  let is_empty t = empty = t
  let show t = t |> List.map Event.show |> String.concat " & "
  let pp formatter t = Format.fprintf formatter "%s" (show t)
    end
  include OrdList
include (Events_common.Make(OrdList) :
    module type of Events_common.Make(OrdList) with module Event := Event)
end
