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

  val observe_data : Data.t -> Events.t -> t -> t Or_error.t

  (** Observe events from a [data] instance of descriptive statistics. This
    can be used to batch updates, or to load independently generated datasets
    together in a meaningful way. The returned model reflects the observation
    updates while the original instance is not guaranteed to be current. *)
end

module Observe_error_converter
  (Error_in : ERROR)
  (Error_out : sig include ERROR val of_observe : Error_in.t -> t end) : ERROR_CONVERTER with
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_observe e
  end
