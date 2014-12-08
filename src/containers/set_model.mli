
(** The event type for each element of a set. *)
module type EVENT = Model_intf.EVENT

(** The type holding a set of events *)
module type EVENTS = Model_intf.EVENTS

(** Represents a polymoprhic cache of event sets mapped to probabilities and expectations *)
module type S = Model_intf.S

(** Creates a concrete instance of an event set model cache for a given Event type *)
module Make : functor(Event:EVENT) -> S with type Events.Event.t = Event.t and module Events.Event = Event 


