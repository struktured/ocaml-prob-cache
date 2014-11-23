module type EVENT = sig type t [@@deriving ord, show] end

module type EVENTS = 
sig
  module Event : EVENT
  type t [@@deriving iter, ord, show]
  val is_empty : t -> bool
  val empty : t
  val union : t -> t -> t
  val of_list : Event.t list -> t
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


module Make_for_events (Events:EVENTS) : S with module S.Events = Events =
struct
  module Event = Events.Event
  module Cache = CCMultiSet.Make(Events)

  type t = {name:string; cache : Cache.t}

  let create name = {name;cache=Cache.empty}

  let count events t = Cache.count t.cache events

(* Computes P(events|conditioned_on) via P(events, conditioned_on) / P(conditioned_on). If conditioned_on is blank it simply becomes the marginal probability P(events) *)
  let prob ?(cond=Events.empty) ~(events:Events.t) t =
    let event_count = count t (Events.union events conditioned_on) in
    let given_cond_count = count t conditioned_on in 
    let cond_count = if given_cond_count = 0 && 
                        (not (Events.is_empty conditioned_on)) then count Events.empty t else given_cond_count in
    if (cond_count = 0) then Lwt.return(Float.of_int 0) else
      (Float.of_int event_count) /. (Float.of_int cond_count)

  let rec add_mult ?(cnt=1) s v = 
    if cnt == 0 then s else add_mult ~cnt:(cnt-1) s v

  let increment ?(cnt=1) t ~(events:Events.t) =
    let cnt = (count model events) in 
    add_mult ~cnt:(cnt + weight) t.cache events

  let observe ?(cnt=1) ~(events:Events.t) t =
    List.iter (powerset (Events.to_list events)) 
      (fun set -> increment ?cnt model (Events.of_list set));
end

module Make(Event:EVENT) = Make_for_events(CCMultiSet.Make(Event))
   
