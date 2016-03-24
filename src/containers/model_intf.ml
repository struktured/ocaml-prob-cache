(** A abstract model, defining the type of events and data structures
to maintain their probabilities and expectations.
*)
open Prob_cache_common

(** Floating point convenience module *)
module Float = CCFloat

(** Represents a single event- must be comparable and showable *)
module type EVENT = sig type t [@@deriving ord, show] end
(** Represents an abstract collection of events *)
module type EVENTS =
sig
  module Event : EVENT
  type t [@@deriving ord]
  val is_empty : t -> bool
  val join: t -> t -> t
  val empty : t
  val of_list : Event.t list -> t
  val to_list : t -> Event.t list
  val subsets : t -> t list
  val show : t -> string
end

module Data(Events: EVENTS) = struct
  module Ord_t =
    struct
      (** Compute running statitics using recurrence equations. *)
      type t = Oml.Online.t = { size : int         (** Number of observations. *)
      ; last : float        (** Last observation. *)
      ; max : float        (** Maxiumum. *)
      ; min : float        (** Minimum. *)
      ; sum : float        (** Sum . *)
      ; sum_sq : float     (** Sum of squares. *)
      ; mean : float      (** Mean. *)
      ; var : float (** _Unbiased_ variance *)
      } [@@deriving show, ord]
    end
  include Data.Make_with_defaults(Events)(Ord_t)
  let compare = Ord_t.compare
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig

  (** The module type representing one event *)
  module Event : EVENT

  (** The module type representing a collection of events *)
  module Events : EVENTS with module Event = Event

  module Data : module type of Data(Events)

  (* Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (* Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

  (** An abstract events model cache *)
  type t

  (** Defines the update rule for expectations *)
  type update_rule = Update_rules.UPDATE_FN(Events).t

  val create : ?prior_count:prior_count -> ?prior_exp:prior_exp -> name:string -> t
  (** Creates a new model cache labeled by the given string. By default, expectations are updated
     using a mean value estimator and all priors are value 0. *)

  val count : Events.t -> t -> int
  (** How many times some particular events were observed *)

  val observe : ?cnt:int -> ?exp:float -> Events.t -> t -> t
  (** Observe events with a default count and expectation of 1.
    The returned model reflects the observation updates
    while the original instance is not guaranteed to be current. *)

  val observe_data : Data.t -> Events.t -> t -> t
  (** Observe events from a [data] instance of descriptive statistics. This
    can be used to batch updates, or to load independently generated datasets
    together in a meaningful way. The returned model reflects the observation
    updates while the original instance is not guaranteed to be current. *)

  val data : ?cond:Events.t -> Events.t -> t -> Data.t option
  (** Gets the desscriptive statistics [data] for the given events conditioned
      on [cond]. Returns [None] if no data exists for the events. *)

  val prob : ?cond:Events.t -> Events.t -> t -> float
  (** Probability of events given [cond], possibly the empty events *)

  val exp : ?cond:Events.t -> Events.t -> t -> float
  (** Expectation of events given [cond], possibly the empty events *)

  val var : ?cond:Events.t -> Events.t -> t -> float
  (** Statistical variance of events given [cond], possibly the empty events *)

  val sum : ?cond:Events.t -> Events.t -> t -> float
  (** Aggregated sum of events given [cond], possibly the empty events *)

  val max : ?cond:Events.t -> Events.t -> t -> float
  (** Observed maximum of events given [cond], possibly the empty events *)

  val min : ?cond:Events.t -> Events.t -> t -> float
  (** Observed minimum of events given [cond], possibly the empty events *)

  val last : ?cond:Events.t -> Events.t -> t -> float
  (** Observed last value of events given [cond], possibly the empty events *)

  val name : t -> string
  (** Gets the name of the cache *)

end
