(** A abstract model, defining the type of events and data structures 
to maintain their probabilities and expectations.
*)
open Prob_cache.Std
module OldList = List
open Core.Std
(** Floating point convenience module *)
module Float = CCFloat (* for pretty printing, ord, etc *)
module CoreList = List
module List = CCList
open Async.Std
open Or_errors_async.Std
(** Represents a single event- must be protobuf capable, comparable, and pretty printable *)
module type EVENT =
sig
  type t [@@deriving protobuf, show, ord]
  include Events.EVENT with type t := t
end

(** Represents an abstract collection of events, must be protobuf capable and pretty printable *)
module type EVENTS =
sig
  type t [@@deriving protobuf, show]
  module Event : EVENT
  include Events.EVENTS with module Event := Event and type t := t
end

module Data =
struct
  module Proto_T =
    struct
      (** Compute running statitics using recurrence equations. *)
      type t = Running.t = { size : (int [@key 1])         (** Number of observations. *)
      ; last : (float [@key 2])       (** Last observation. *)
      ; max : (float [@key 3])       (** Maxiumum. *)
      ; min : (float [@key 4])       (** Minimum. *)
      ; sum : (float [@key 5])   (** Sum . *)
      ; sum_sq : (float [@key 6])    (** Sum of squares. *)
      ; mean : (float [@key 7])      (** Mean. *)
      ; var : (float [@key 8])      (** _Unbiased_ variance. *)
      } [@@deriving show, protobuf, ord]
    end

  include Data.Make(Proto_T)

  let from_protobuf = Proto_T.from_protobuf
  let to_protobuf = Proto_T.to_protobuf
end

module type S_KERNEL =
sig
    module Events : EVENTS
    include Model_kernel.S with
      module Or_error = Or_error and
      module Events := Events
end

module type S =
sig
    module Events : EVENTS
    (*module Event = Events.Event*)
    module Or_error : module type of Or_error
    include Model_decorator.S with
      module Events := Events and
(*      module Event := Event and*)
      module Or_error := Or_error
end
