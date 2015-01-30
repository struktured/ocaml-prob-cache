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

module Data = struct
  module Proto_T = struct type t = {cnt:int [@key 1];exp:float [@key 2]} [@@deriving protobuf, show] end
  include Data.Make(Proto_T)

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

  (** The riak cache backing the probability model. *)
  module Cache : module type of Cache.Make(Events)(Data)

  (** Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (** Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

  (** A probability model cache *)
  type t

  (** Defines the update rule for expectations *)
  type update_rule = t Update_rules.Update_fn.t

  val count : Events.t -> t -> (int, [> Opts.Get.error]) Deferred.Result.t
  (** How many times [events] was observed for the model cache [t].
      Errors during the riak fetch routine are propogated back in the deferred result. *)

  val observe : ?cnt:int -> ?exp:float -> Events.t -> t -> 
    (t, [> Opts.Put.error | Opts.Get.error | Conn.error ]) Deferred.Result.t
  (** Observe events with a default count and expectation of 1. *)

  val prob : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Deferred.Result.t  
  (** Probability of events given observed events, possibly the empty events *)

  val exp : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Deferred.Result.t
  (** Expectation of events given observed events, possibly the empty events *)

  val name : t -> string
  (** Gets the name of the cache *)

  val with_model : ?update_rule:update_rule -> ?prior_count:prior_count -> ?prior_exp:prior_exp -> 
    host:string -> port:int -> name:string -> 
    (t -> ('a, [> Conn.error] as 'e) Deferred.Result.t) -> 
         ('a, 'e) Deferred.Result.t
  (** Execute a deferred function for the specified model where [name] corresponds to a riak bucket for
     the given [host] and [port]. Can optionally specify custom update rules or prior functions. *) 
end





   
