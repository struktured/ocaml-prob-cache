open Prob_cache_common
module Float = CCFloat

(** Represents a single event- must be comparable and showable *)  
module type EVENT = sig type t [@@deriving ord, show] end

(** Represents an abstract collection of events *)
module type EVENTS = 
sig
  module Event : EVENT
  type t [@@deriving ord]
  val is_empty : t -> bool
  val join: t -> t -> t
  val empty : t
  val of_list : Event.t list -> t
  val to_list : t -> Event.t list
  val subsets : t -> t list
end


(** A module type provided polymorphic sequence model caches *)
module type S =
sig
  (** The module type representing a discrete sequence *)
  module Events : EVENTS

  (** The module type representing one event in a sequence *)
  module Event : module type of Events.Event

  (** Defines the update rule for expectations *)
  type update_rule = Update_rules.Update_fn.t

  (* Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (* Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

  (** A sequence model cache *)
  type t
  
  val create : ?update_rule:update_rule -> ?prior_count:prior_count -> ?prior_exp:prior_exp -> name:string -> t      
  (** Creates a new sequence model labeled by the given string. By default, expectations are updated 
     using a mean value estimator and all priors are value 0. *)

  val count : Events.t -> t -> int
  (** How many times a particular sequence was observed *)

  val observe : ?cnt:int -> ?exp:float -> Events.t -> t -> t
  (** Observe a sequence with a default count and expectation of 1. 
    The returned model reflects the observation updates
    while the original instance is not guaranteed to be current. *)

  val prob : ?cond:Events.t -> Events.t -> t -> float
  (** Probability of events given observed events, possibly the empty events *)

  val exp : ?cond:Events.t -> Events.t -> t -> float
  (** Expectation of events given observed events, possibly the empty events *)

  val name : t -> string
  (** Gets the name of the given sequence model *)

end

module Data = 
  struct
    type t = {cnt:int; exp:float} [@@deriving ord, show]

    let count t = t.cnt
    let expect t = t.exp

    let update ?(cnt=1) ?(exp=1.0) (t:t option) = CCOpt.get {cnt;exp} (CCOpt.map (fun t -> {cnt=(count t) + cnt;exp=(expect t) +. exp}) t)
  end


module Make_for_events (Events:EVENTS) : S with module Events = Events =
struct
  module Events = Events
  module Event = Events.Event
  module Cache = CCMap.Make(Events)
  module Int = CCInt
  
  type prior_count = Events.t -> int
  type prior_exp = Events.t -> float

  type update_rule = Update_rules.Update_fn.t

  type t = {
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
    let d = Data.update ~cnt ~exp (Cache.get events t.cache) in
    {t with cache=Cache.add events d t.cache}

  let observe ?(cnt=1) ?(exp=1.0) (events:Events.t) (t:t) : t =
    List.fold_right (fun l t -> increment ~cnt ~exp l t) (Events.subsets events) t

  let name t = t.name
end

module Make_event_set(Event:EVENT) = 
struct
  module Multiset = CCMultiSet.Make(Event)
  include Multiset
  module Event = Event

  let join = union
  let subsets t = List.map of_list (Util.powerset (to_list t))
end


module Make_event_sequence(Event:EVENT) =
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

module Make_sequence(Event:EVENT) = Make_for_events(Make_event_sequence(Event))
module Make_set(Event:EVENT) = Make_for_events(Make_event_set(Event))


   
