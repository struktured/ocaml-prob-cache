(** A abstract model, defining the type of events and data structures
to maintain their probabilities and expectations.
*)

(** Floating point convenience module *)
module Float = CCFloat
open Events_common

module type MONAD =
  sig 
    type 'a t 
    val map :  ('a -> 'b) -> 'a t -> 'b t
    val bind : ('a -> 'b t) -> 'a t -> 'b t
    val return : 'a -> 'a t

    module Infix :
      sig 
        val (>>|) : ('a -> 'b) -> 'a t -> 'b t
        val (>|=) : ('a -> 'b) -> 'a t -> 'b t
        val (>>=) : ('a -> 'b t) -> 'a t -> 'b t
      end
  end

module type RESULT = 
  sig 
    type ('ok, 'err) t = Ok of 'ok | Error of 'err 
    val map : ('ok -> 'res) -> ('ok, 'err) t -> ('res, 'err) t
    val bind : ('ok -> ('res, 'err) t) -> ('ok, 'err) t -> ('res, 'err) t
    module Monad_infix :
      sig
        val (>>|) : ('ok, 'err) t -> ('ok -> 'res) -> ('res, 'err) t
        val (>|=) : ('ok, 'err) t -> ('ok -> 'res) ->  ('res, 'err) t
        val (>>=) : ('ok, 'err) t -> ('ok -> ('res, 'err) t) -> ('res, 'err) t
      end
  end

module type ERROR =
sig
  type t
  val to_string_hum : t -> string
  val to_string_mach : t -> string
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig

  module Result : RESULT
  
  module Create_error : ERROR
  module Observe_error : ERROR
  module Data_error : ERROR
  module Events_error : ERROR

  module Or_error :
  sig
    type 'a t = ('a, 
      [ `Create_error of Create_error.t 
      | `Observe_error of Observe_error.t
      | `Data_error of Data_error.t
      | `Events_error of Events_error.t]) Result.t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val bind : ('a -> 'b t) -> 'a t -> 'b t
    val return : 'a -> 'a t
    module Monad_infix :
      sig
        val (>>|) : 'a t -> ('a -> 'b) -> 'b t
        val (>|=) : 'a t -> ('a -> 'b) ->  'b t
        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      end
  end
  
  (** The module type representing one event *)
  module Event : EVENT

  (** The module type representing a collection of events *)
  module Events : EVENTS with module Event = Event

  module Data : Data.S

  (* Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (* Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

  (** An abstract events model cache *)
  type t

  (** Defines the update rule for expectations *)
  type update_rule = Events.t Data.update_rule

  val update_rule : t -> update_rule

  val create : ?update_rule:update_rule -> ?prior_count:prior_count -> ?prior_exp:prior_exp -> name:string -> 
    (t, Create_error.t) Result.t
  (** Creates a new model cache labeled by the given string. By default, expectations are updated
     using a mean value estimator and all priors are value 0. *)

  val observe : Data.t -> Events.t -> t -> (t, Observe_error.t) Result.t
  (** Observe events from a [data] instance of descriptive statistics. This
    can be used to batch updates, or to load independently generated datasets
    together in a meaningful way. The returned model reflects the observation
    updates while the original instance is not guaranteed to be current. *)

  val data : Events.t -> t -> (Data.t option, Data_error.t) Result.t
  (** Gets the descriptive statistics data for the given events. 
      Returns [None] if no data exists for the events. *)

  val events : (Events.t -> bool) -> t -> (Events.t, Events_error.t) Result.t
  (** Gets all observed events given a filter function from the model. *)

  val complement : Events.t -> t -> (Events.t, Events_error.t) Result.t
  (** Gets the empirical complement of the given events. *)
  
  val name : t -> string
  (** Gets the name of the cache *)

 
  (** Simpler interface that unifies the error type *)
  module Or_errors :
    sig
      type update_rule = Events.t Data.update_rule
      val update_rule : t -> update_rule
      val create : ?update_rule:update_rule -> ?prior_count:prior_count -> ?prior_exp:prior_exp -> name:string -> 
        t Or_error.t
      val observe : Data.t -> Events.t -> t -> t Or_error.t
      val data : Events.t -> t -> Data.t option Or_error.t
      val events : (Events.t -> bool) -> t -> Events.t Or_error.t
      val complement : Events.t -> t -> Events.t Or_error.t
      val name : t -> string
  end

end
