open Or_errors.Std
open Events_common
module type DATA = Data.S
module type S =
  sig

  type t
  module Find_or_error : OR_ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  type find = (Events.t -> bool) -> t -> Events.t Find_or_error.t
  (** Gets all observed events given a filter function from the model. *)

  val find : find
  (** Gets all observed events given a filter function from the model. *)

  end


