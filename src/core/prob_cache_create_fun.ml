open Or_errors.Std
open Prob_cache_events
module Data = Prob_cache_data
module type DATA = Data.S
(** Creation function signature for all probability cache implementations *)
module type S =
  sig

  (** The type of the probability cache *)
  type t

  (** Or_error module implementation for this cache *)
  module Or_error : OR_ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  module Options : Prob_cache_options.S

  (** Creates a new model cache labeled by the given string. By default, expectations are updated
     using a mean value estimator and all priors are value 0. *)
  val with_cache : ?opt:Options.t -> (t -> 'a Or_error.t) ->
    'a Or_error.t

  val name : t -> string
  (** Gets the name of the cache *)

end

module Create_error_converter
  (Error_in : ERROR)
  (Error_out :
   sig include ERROR val of_create : Error_in.t -> t end) :
  ERROR_CONVERTER with
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_create e
  end
