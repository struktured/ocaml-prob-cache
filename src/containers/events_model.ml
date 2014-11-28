open Prob_cache_common
module type EVENT = sig type t [@@deriving ord, show] end

module type EVENTS = 
sig
  module Event : EVENT
  type t [@@deriving ord]
  val is_empty : t -> bool
  val empty : t
  val union : t -> t -> t
  val of_list : Event.t list -> t
  val to_list : t -> Event.t list
  val add_mult : ?cnt:int -> t -> Event.t -> t
end


module type S =
sig
  module Events : EVENTS
  module Event : module type of Events.Event
  type t
  val count : Events.t -> t -> int
  val create : string -> t
  val observe : ?cnt:int -> Events.t -> t -> t 
  val prob : ?cond:Events.t -> Events.t -> t -> float
  val name : t -> string
end


module Make_for_events (Events:EVENTS) : S with module Events = Events =
struct
  module Events = Events
  module Event = Events.Event
  module Cache = CCMultiSet.Make(Events)

  type t = {name:string; cache : Cache.t}

  let create name = {name;cache=Cache.empty}

  let count events t = Cache.count t.cache events

  let prob ?(cond=Events.empty) (events:Events.t) t =
    if cond = events then float_of_int 1 else
    let event_count = count (Events.union events cond) t in 
    if event_count = 0 then (float_of_int 0) else
    let given_cond_count = count cond t in
    (* (a) If the conditional event has never been observed then it has no effect on the probability
     * (b) If the conditional event was observed we normalize by its frequency count
     * (c) If the conditional event was empty we normalize by the total frequnecy count (on the empty event set) *)
    let cond_count = if given_cond_count = 0 && 
                        (not (Events.is_empty cond)) then count Events.empty t else given_cond_count in
    if (cond_count = 0) then (float_of_int 0) else
      (float_of_int event_count) /. (float_of_int cond_count)
  
  let rec add_mult ?(cnt=1) c v : Cache.t = 
    if cnt = 0 then c else add_mult ~cnt:(cnt-1) (Cache.add c v) v

  let increment ?(cnt=1) (events:Events.t) t =
    {name = t.name; cache=add_mult ~cnt t.cache events}

  let observe ?(cnt=1) (events:Events.t) t =
    let ps = Util.powerset (Events.to_list events) in
    List.fold_left (fun t set -> increment ~cnt (Events.of_list set) t) t ps

  let name t = t.name
end


module Make_events(Event:EVENT) = 
struct
  module Multiset = CCMultiSet.Make(Event)
  include Multiset
  module Event = Event

  let rec add_mult ?(cnt=1) s v : t = 
    if cnt = 0 then s else add_mult ~cnt:(cnt-1) (add s v) v

end

module Make(Event:EVENT) = Make_for_events(Make_events(Event))

   
