(** 
 * A cache for sequence oriented probability models 
 * The event type is polymorphic but depends on order semantics.
 * Use [@@deriving ord] to easily support this with your own types.
 *
 * Suppose we observe a sequence of events e_0, e_1, e_2, e_3
 * We now want to now the likelihoods of future observations e_4, e_5 or 
 * P(e_4,e_5|e_0, e_1, e_2, e_3) 
 * 
 * More concretely:
   * If we observe RED | GREEN | GREEN | BLUE | RED | GREEN
   * then our universe is the following
   RED - > 2
   RED | GREEN -> 2
   GREEN -> 3
   GREEN | GREEN -> 1
   RED | GREEN | GREEN -> 1
   BLUE -> 1
   GREEN | BLUE -> 1
   GREEN | GREEN | BLUE -> 1
   RED | GREEN | GREEN | BLUE -> 1
   BLUE | RED -> 1
   BLUE | RED | GREEN - > 1
   RED | GREEN | GREEN | BLUE | RED | GREEN -> 1
   GREEN | GREEN | BLUE | RED | GREEN -> 1
   GREEN | GREEN | BLUE | RED -> 1
   GREEN | BLUE | RED -> 1

   Complexity Overview:

   1) Total number of sequences we have to update given a new sequence of length of L is O(L),
      A frequency count is maintained for each subsequence of which there are L of them.
   2) If we cache each sequence independently we would get E^L + E^(L-1) + E^(L-2) .. entries,
      eg. O(E^L) in the worst case (E is cardinality of the event type). Ideally we would 
      observe much less than that, but this is also not the most efficient 
      representation, either.

      We can at least a little more efficiently encode the data by using a graphical representation.
      For instance, the observation RED | GREEN | RED results in this graphical encoding:
        [(RED, 2, [(GREEN, 1, [(RED, 1, [])])]);
        (GREEN, 1, [(RED, 1, [])])]
      The benefit is the key for GREEN | RED and is composed of the KEY for GREEN, which saves space referencing GREEN directly.
      While the complexity doesn't theoretically change much, in practice this can be a big win. Currently NOT implemented.
**)
open Prob_cache_common
module Float = CCFloat

(** Represents a single event- must be comparable and showable *)  
module type EVENT = sig type t [@@deriving ord, show] end

(** Represents a discrete sequence of events *)
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
  
  (** A sequence model cache *)
  type t
  
  val create : string -> t      
  (** Creates a new sequence model labeled by the given string *)

  val count : Events.t -> t -> int
  (** How many times a particular sequence was observed *)

  val observe : ?cnt:int -> ?exp:float -> Events.t -> t -> t
  (** Observe a sequence with a default count and expectation of 1. 
   * The returned model reflects the observation updates
   * while the original instance is not guaranteed to be current. *)

  val prob : ?cond:Events.t -> Events.t -> t -> float
  (** Probability of future sequence given an observed sequence, possibly the empty seqence *)

  val exp : ?cond:Events.t -> Events.t -> t -> float
  (** Expectation of future sequence given an observed sequence, possibly the empty sequence *)

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

  type t = {name:string; cache : Data.t Cache.t}

  let create name = {name;cache=Cache.empty}

  let count (sequence:Events.t) (t:t) : int = 
    CCOpt.get 0 (CCOpt.map (fun d -> Data.count d) (Cache.get sequence t.cache))

  let exp ?(cond=Events.empty) (sequence:Events.t) (t:t) : float = 
    let full_seq = Events.join cond sequence in
    CCOpt.get 0.0 (CCOpt.map (fun d -> Data.expect d) (Cache.get full_seq t.cache))

  let prob ?(cond=Events.empty) (sequence:Events.t) (t:t) =
   let cond_count = count cond t in
    (* (a) If the conditional sequence has NOT been observed then probability must be zero
     * (b) If the conditional (possibly empty) sequence has been observed then we normalize by its frequency count 
     *)
    if (cond_count = 0) then (Float.of_int 0) else
    let full_seq_count = count (Events.join cond sequence) t in
    (Float.of_int full_seq_count) /. (Float.of_int cond_count)
 
  let increment ?(cnt=1) ?(exp=1.0) (sequence:Events.t) (t:t) =
    let d = Data.update ~cnt ~exp (Cache.get sequence t.cache) in
    {name = t.name; cache=Cache.add sequence d t.cache}

  let observe ?(cnt=1) ?(exp=1.0) (sequence:Events.t) (t:t) : t =
    List.fold_right (fun l t -> increment ~cnt ~exp l t) (Events.subsets sequence) t

  let name t = t.name
end

module Make_event_set(Event:EVENT) = 
struct
  module Multiset = CCMultiSet.Make(Event)
  include Multiset
  module Event = Event

  let rec add_mult ?(cnt=1) s v : t = 
    if cnt = 0 then s else add_mult ~cnt:(cnt-1) (add s v) v
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


   
