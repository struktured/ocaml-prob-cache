
(** The event type for each element of a sequence. *)
module type EVENT = Model_intf.EVENT

(** The type holding a sequence of events *)
module type EVENTS = Model_intf.EVENTS

(** Represents a polymoprhic cache of sequences mapped to probabilities and expectations *)
module type S = Model_intf.S

(** Creates a concrete instance of a sequence model cache for a given Event type *)
module Make : module type of Model_intf.Make_sequence

