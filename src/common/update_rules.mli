module type WEIGHT = sig val value : float end

module Update_fn : 
sig
  type 'a t = ?orig:float -> obs:float -> cnt:int -> 'a -> float
end

module type S = 
sig
  val update : 'a Update_fn.t
end

module type WEIGHT_PROVIDER = 
sig 
  val weight : 'a Update_fn.t
end

module Make_weighted : functor(Weight_provider:WEIGHT_PROVIDER) -> S

module Constant : functor(Weight:WEIGHT) -> S

module Mean : S

val constant : float -> 'a Update_fn.t
val mean : 'a Update_fn.t
