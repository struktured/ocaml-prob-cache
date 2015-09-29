open Or_errors.Std
open Events_common
module type DATA = Data.S
module type S =
  sig

  type t

  module Observe_data_or_error : OR_ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  type observe_data = Data.t -> Events.t -> t -> t Observe_data_or_error.t

  val observe_data : observe_data
  (** Observe events from a [data] instance of descriptive statistics. This
    can be used to batch updates, or to load independently generated datasets
    together in a meaningful way. The returned model reflects the observation
    updates while the original instance is not guaranteed to be current. *)
end
