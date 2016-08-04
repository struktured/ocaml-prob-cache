(** A distributed riak backed cache for set oriented probability models *)
(**
  The event type is polymorphic but depends on protobuf serialization (as well as show and ord).
  Use \[\@\@deriving protobuf,show,ord\] to easily support this with your own types.

  Suppose we observe co-occurences of events A, B, C, and we want to estimate joint
  or conditional probabilities among them.

  More concretely:
    If we observe \[RED, GREEN\], \[RED\], \[RED, BLUE\]
    then the following is true:

   P(RED) = 1

   P(RED|GREEN) = 1

   P(BLUE) = 1/3

   P(RED|BLUE) = 1

   P(BLUE|GREEN) = 0

   Complexity Overview:

   The total number of sets we have to update given a new set of cardinality of L is O(2^L),
   A frequency count is maintained for each subset of which there are 2^L of them.
*)


(** The event type for each element of a set. *)
module type EVENT = Riak_model_intf.EVENT

(** The type holding a set of events *)
module type EVENTS = Riak_model_intf.EVENTS

(** Represents a polymoprhic cache of event sets mapped to probabilities and expectations *)
module type S = Riak_model_intf.S

(** Creates a concrete instance of an event set model cache for a given Event type *)
module Make : functor(Event:EVENT) -> S with type Events.Event.t = Event.t and module Events.Event = Event


