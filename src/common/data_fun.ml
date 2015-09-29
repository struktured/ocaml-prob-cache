open Or_errors.Std
open Events_common
module type DATA = Data.S
module type S =
sig

  type t
  module Data_or_error : OR_ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  type data = Events.t -> t -> Data.t Data_or_error.t

  val data : data
  (** Gets the descriptive statistics data for the given events.
      Returns data with count of zero otherwise and other values set to nan. *)

end
