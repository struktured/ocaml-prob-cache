module type WEIGHT = sig val value : float end

module Update_fn : 
sig
  type t = ?orig:float -> obs:float -> cnt:int -> float
end

module type S = 
sig
  val update : t
end

module type WEIGHT_PROVIDER = 
sig 
  val weight : Update_fn.t
end

module Make_weighted : functor(Weight_provider:WEIGHT_PROVIDER) -> S

module Constant : functor(Weight:WEIGHT) -> S

module Mean : S

val constant : float -> Update_fn.t
val mean : Update_fn.t
