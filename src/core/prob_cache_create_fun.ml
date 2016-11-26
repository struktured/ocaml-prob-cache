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

  (** Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (** Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

  (** Defines the update rule for expectations *)
  type update_rule = Events.t Data.update_rule
  val update_rule : t -> update_rule

  val create :
      ?update_rule:update_rule ->
      ?prior_count:prior_count ->
      ?prior_exp:prior_exp ->
      name:string ->
      t Or_error.t
  (** Creates a new model cache labeled by the given string. By default, expectations are updated
     using a mean value estimator and all priors are value 0. *)

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
