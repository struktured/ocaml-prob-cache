module type EVENTS = Prob_cache_events.EVENTS
module type DATA = Prob_cache_data.DATA
module Options = Prob_cache_options


(**
* Priors are defined functionally. When observations are
* found which have yet to be observed, the functional priors
* determine initial frequency counts and expectations *)
module type S =
sig

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA


  (** Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (** Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float
  class prior : prior_exp -> prior_count ->
    object
      method prior_exp : prior_exp
      method prior_count : prior_count
    end

  type t = prior (*<prior_count:prior_count; prior_exp:prior_exp>*)

  include Options.S with type t := t
end

(** Creates a new prior signature given the events and data signatures *)
module Make(Events:EVENTS)(Data:DATA) :
  S with module Events := Events and module Data := Data =
struct
  (** The module type representing a collection of events *)
  module Events = Events

  (** Container for the descriptive statistics **)
  module Data = Data

  (** Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (** Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

    class prior(prior_exp:prior_exp) (prior_count:prior_count) =
    object(self)
      method prior_exp = prior_exp
      method prior_count = prior_count
    end
  type t = prior
let default = new prior (fun _ -> 0.0) (fun _ -> 1)

end

