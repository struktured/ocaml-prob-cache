(** A containers backed cache for set oriented probability models *)
(** 
  The event type is polymorphic but depends on order semantics. (as well as show).
  Use \[\@\@deriving show,ord\] to easily support this with your own types.
 
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
module type EVENT = Model_intf.EVENT

(** The type holding a set of events *)
module type EVENTS = Model_intf.EVENTS

(** Represents a polymoprhic cache of event sets mapped to probabilities and expectations *)
module type S = Model_intf.S

(** Creates a concrete instance of an event set model cache for a given Event type *)
module Make : functor(Event:EVENT) -> S with module Events.Event = Event


