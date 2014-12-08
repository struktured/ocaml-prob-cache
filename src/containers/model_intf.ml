open Prob_cache_common
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
end


(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig
  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** The module type representing one event *)
  module Event : module type of Events.Event

  (** Defines the update rule for expectations *)
  type update_rule = Update_rules.Update_fn.t

  (* Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (* Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

  (** A sequence model cache *)
  type t
  
  val create : ?update_rule:update_rule -> ?prior_count:prior_count -> ?prior_exp:prior_exp -> name:string -> t      
  (** Creates a new sequence model labeled by the given string. By default, expectations are updated 
     using a mean value estimator and all priors are value 0. *)

  val count : Events.t -> t -> int
  (** How many times a particular sequence was observed *)

  val observe : ?cnt:int -> ?exp:float -> Events.t -> t -> t
  (** Observe a sequence with a default count and expectation of 1. 
    The returned model reflects the observation updates
    while the original instance is not guaranteed to be current. *)

  val prob : ?cond:Events.t -> Events.t -> t -> float
  (** Probability of events given observed events, possibly the empty events *)

  val exp : ?cond:Events.t -> Events.t -> t -> float
  (** Expectation of events given observed events, possibly the empty events *)

  val name : t -> string
  (** Gets the name of the cache *)

end

(** Holds statistical data for one observation *)
module type Data = 
  sig
    type t = {cnt:int; exp:float} [@@deriving ord, show]
  
    val count:  t -> int 
    val expect: t -> float

    val update : ?cnt:int -> ?exp:float -> t option -> t 
  end

