(** Represents a single event- must be showable *)
module type EVENT = sig type t [@@deriving show] end

(** Represents an abstract collection of events *)
module type EVENTS_BASE =
sig
(*  module type EVENT = EVENT *)
  module Event : EVENT
  type t [@@deriving show]
  val is_empty : t -> bool
  val join: t -> t -> t
  val empty : t
  val of_list : Event.t list -> t
  val to_list : t -> Event.t list
  val subsets : t -> t list
  val add : t -> Event.t -> t
  val remove : t -> Event.t -> t
  val filter : (Event.t -> bool) -> t -> t
  val fold : (Event.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val iter : (Event.t -> unit) -> t -> unit
end

(** Represents an abstract collection of events *)
module type EVENTS =
sig
  module Events_base : EVENTS_BASE
  include module type of Events_base
  module Infix :
  sig
   val ($) : Event.t -> (t -> 'a) -> 'a
   val ($$) : Event.t list -> (t -> 'a) -> 'a
   val (&) : t -> t -> t
   val (+=) : t -> Event.t -> t
   val (-=) : t -> Event.t -> t
   val (^^) : t -> (Event.t list -> 'a) -> 'a
  end
end

module Make : functor (Events:EVENTS_BASE) -> EVENTS
  with module Event = Events.Event and module Events_base = Events

