(** A abstract model, defining the type of events and data structures 
to maintain their probabilities and expectations.
*)
open Prob_cache_common
module OldList = List
open Core.Std

(** Floating point convenience module *)
module Float = CCFloat (* for pretty printing, ord, etc *)
module CoreList = List
module List = CCList
open Async.Std
module Result = Deferred.Result
(** Represents a single event- must be protobuf capable, comparable, and pretty printable *)
module type EVENT =
sig
  type t [@@deriving protobuf, show, ord]
  include Events_common.EVENT with type t := t
end

(** Represents an abstract collection of events, must be protobuf capable and pretty printable *)
module type EVENTS =
sig
  type t [@@deriving protobuf, show]
  module Event : EVENT
  include Events_common.EVENTS with module Event := Event and type t := t
end

module Data(Events:EVENTS) = struct
  module Proto_T =
    struct
      (** Compute running statitics using recurrence equations. *)
      type t = Oml.Online.t = { size : int [@key 1]         (** Number of observations. *)
      ; last : float [@key 2]       (** Last observation. *)
      ; max : float [@key 3]       (** Maxiumum. *)
      ; min : float [@key 4]       (** Minimum. *)
      ; sum : float [@key 5]       (** Sum . *)
      ; sum_sq : float [@key 6]    (** Sum of squares. *)
      ; mean : float [@key 7]      (** Mean. *)
      ; var : float [@key 8]       (** _Unbiased_ variance. *)
      } [@@deriving show, protobuf]
    end

  include Data.Make(Events)(Proto_T)

  let from_protobuf = Proto_T.from_protobuf
  let to_protobuf = Proto_T.to_protobuf
end



(** A module type provided polymorphic probability model caches. Uses in distributed models backed by riak *)
module type S =
sig
  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** The module type representing one event *)
  module Event : module type of Events.Event

  module Data : module type of Data(Events)

  (** The riak cache backing the probability model. *)
  module Cache : module type of Cache.Make(Events)(Data)

  (** Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (** Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

  (** A probability model cache *)
  type t

  (** Defines the update rule for expectations *)
  type update_rule = Update_rules.UPDATE_FN(Events).t

  val count : Events.t -> t -> (int, [> Opts.Get.error]) Result.t
  (** How many times [events] was observed for the model cache [t].
      Errors during the riak fetch routine are propogated back in the deferred result. *)

  val observe : ?cnt:int -> ?exp:float -> Events.t -> t ->
    (t, [> Opts.Put.error | Opts.Get.error | Conn.error ]) Result.t
  (** Observe events with a default count and expectation of 1. *)

  val observe_data : Data.t -> Events.t -> t ->
    (t, [> Opts.Put.error | Opts.Get.error | Conn.error ]) Result.t

  val data : ?cond:Events.t -> Events.t -> t -> (Data.t option, [> Opts.Get.error]) Result.t
  (** Gets the desscriptive statistics [data] for the given events conditioned
      on [cond]. Returns [None] if no data exists for the events. *)

  val prob : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Result.t
  (** Probability of events given observed events, possibly the empty events *)

  val exp : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Result.t
  (** Expectation of events given [cond], possibly the empty events *)

  val var : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Result.t
  (** Statistical variance of events given [cond], possibly the empty events *)

  val sum : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Result.t
  (** Aggregated sum of events given [cond], possibly the empty events *)

  val max : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Result.t
  (** Observed maximum of events given [cond], possibly the empty events *)

  val min : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Result.t
  (** Observed minimum of events given [cond], possibly the empty events *)

  val last : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Result.t
  (** Observed last value of events given [cond], possibly the empty events *)

  val name : t -> string
  (** Gets the name of the cache *)

  val with_model : ?update_rule:update_rule -> ?prior_count:prior_count -> ?prior_exp:prior_exp ->
    host:string -> port:int -> name:string ->
    (t -> ('a, [> Conn.error] as 'e) Result.t) ->
         ('a, 'e) Result.t
  (** Execute a deferred function for the specified model where [name] corresponds to a riak bucket for
     the given [host] and [port]. Can optionally specify custom update rules or prior functions. *)
end
