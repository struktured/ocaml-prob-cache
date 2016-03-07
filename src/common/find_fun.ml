open Or_errors.Std
open Events_common
module type DATA = Data.S
module type S =
  sig

  type t
  module Or_error : OR_ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  val find : (Events.t -> bool) -> t -> Events.t Or_error.t
  (** Gets all observed events given a filter function from the model. *)

  end

module Find_error_converter
  (Error_in : ERROR)
  (Error_out : sig include ERROR val of_find : Error_in.t -> t end) : ERROR_CONVERTER with
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_find e
  end
