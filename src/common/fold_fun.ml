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

  module Make : functor(Accum:sig type t end) ->
  sig
    val fold :
      f:(Accum.t -> Events.t -> bool) ->
      ?filter:(Events.t -> bool) option ->
      ?order_by:(Events.t -> Events.t -> int) ->
      t ->
      init: Accum.t ->
      Accum.t Or_error.t
  end
  (** Gets all observed events given a filter function from the model. *)

  end

module Fold_error_converter
  (Error_in : ERROR)
  (Error_out : sig
    include ERROR
    val of_fold : Error_in.t -> t end) : ERROR_CONVERTER with
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_fold e
  end
