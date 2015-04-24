open Prob_cache_common
module Float = CCFloat

(** Represents a single event- must be comparable and showable *)  
module type EVENT = Model_intf.EVENT 

(** Represents an abstract collection of events *)
module type EVENTS = Model_intf.EVENTS

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S = Model_intf.S

module Data = Model_intf.Data

module Make_for_events (Events:EVENTS) : S with module Event = Events.Event =
struct
  module Event = Events.Event
  module Events = Events
  module Cache = CCMap.Make(Events)
  module Int = CCInt

  type prior_count = Events.t -> int
  type prior_exp = Events.t -> float

  type update_rule = Events.t Update_rules.Update_fn.t

  and t = {
    name : string;
    cache : Data.t Cache.t;
    prior_count : prior_count;
    prior_exp : prior_exp;
    update_rule : update_rule }

  let default_prior_count (e:Events.t) = 0

  let default_prior_exp (e:Events.t) = 0.

  let default_update_rule : update_rule = Update_rules.mean

  let create ?(update_rule=default_update_rule) ?(prior_count=default_prior_count) 
    ?(prior_exp=default_prior_exp) ~(name:string) : t = {name;cache=Cache.empty;prior_count;prior_exp;update_rule}

  let count (events:Events.t) (t:t) : int =
    CCOpt.get_lazy (fun () -> t.prior_count events) (CCOpt.map (fun d -> Data.count d) (Cache.get events t.cache))

  let exp ?(cond=Events.empty) (events:Events.t) (t:t) : float = 
    let joined_events = Events.join cond events in
    CCOpt.get_lazy (fun () -> t.prior_exp events) (CCOpt.map (fun d -> Data.expect d) (Cache.get joined_events t.cache))

  let prob ?(cond=Events.empty) (events:Events.t) (t:t) =
   let cond_count = count cond t in
    (* (a) If the conditional probability is zero then is so must been the whole probability 
       (b) If the conditional (possibly empty) events have been observed then we normalize by its frequency count 
     *)
    if (cond_count = 0) then (Float.of_int 0) else
    let joined_events_count = count (Events.join cond events) t in
    (Float.of_int joined_events_count) /. (Float.of_int cond_count)
 
  let increment ?(cnt=1) ?(exp=1.0) (events:Events.t) (t:t) =
    let d = Data.update ~cnt ~exp ~update_rule:t.update_rule ~prior_count:t.prior_count
      ~prior_exp:t.prior_exp events (Cache.get events t.cache) in
    {t with cache=Cache.add events d t.cache}

  let observe ?(cnt=1) ?(exp=1.0) (events:Events.t) (t:t) : t =
    List.fold_right (fun l t -> increment ~cnt ~exp l t) (Events.subsets events) t

  let name t = t.name
end

module Make_event_set(Event:EVENT) : EVENTS with module Event = Event = 
struct
  module Multiset = CCMultiSet.Make(Event)
  include Multiset
  module Event = Event

  let join = union
  let subsets t = List.map of_list (Util.powerset (to_list t))
end


module Make_event_sequence(Event:EVENT) : EVENTS with module Event = Event =
struct
  module Event = Event
  type t = Event.t list [@@deriving ord]
  let of_list l = l
  let to_list l = l
  let join = CCList.append
  let empty = CCList.empty
  let subsets (l:t) = let (accum, _ ) = 
    List.fold_left 
      (fun ((accum: t list), (l:t)) e -> let l' = l@[e] in (l'::accum, l')) 
      ([], []) 
      l
  in
  []::accum
  let is_empty t = empty = t
end

   
