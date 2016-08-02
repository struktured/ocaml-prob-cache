open Or_errors.Std
open Prob_cache_events
module Data = Prob_cache_data
module type DATA = Data.S
module Fold = Prob_cache_fold
(** Defines a folding function over a cache *)
module type S =
sig

  (** The type of the cache to fold over *)
  type t

  (** Folds are wrapped with error monad *)
  module Or_error : OR_ERROR

  (** The type of entries to fold over *)
  module Entry : Prob_cache_entry.S

  (** Contains the signature for user provided fold functions *)
  module Fold : Fold.S with module Entry = Entry (* and module Or_error = Or_error *)

  val fold : t ->
    state:'f ->
    init:'acc ->
    f:('f, 'acc) Fold.t ->
    'acc Or_error.t
  (** Fold over the cache [t] given the fold function [f] with
        state [state] and initial value [init]. Return the
        accumulated value or error. *)
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
