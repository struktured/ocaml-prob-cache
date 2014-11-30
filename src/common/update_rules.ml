module Float = CCFloat

module type WEIGHT = sig val value : float end

module Update_fn = struct
  type t = ?orig:float -> obs:float -> cnt:int -> float
end

module type S = 
sig
  val update : Update_fn.t
end

module type WEIGHT_PROVIDER = 
sig 
  val weight : Update_fn.t
end

module Mean_weight_provider : WEIGHT_PROVIDER = 
struct
  let weight ?orig ~obs ~cnt = 1.0 /. (Float.of_int cnt)
end

module Constant_weight_provider(Weight:WEIGHT) : WEIGHT_PROVIDER = 
struct
  let weight ?orig ~obs ~cnt = Weight.value
end

module Make_weighted(Weight_provider:WEIGHT_PROVIDER) = 
struct
  module Weight_provider = Weight_provider
  let update ?(orig=0.) ~obs ~cnt  = 
    if (cnt = 0) then obs else
      orig +. (obs -. orig) *. Weight_provider.weight ~obs ~orig ~cnt  
end

module Constant(Weight:WEIGHT) = Make_weighted(Constant_weight_provider(Weight))
module Mean = Make_weighted(Mean_weight_provider)

let constant v = 
  let module C = Constant(struct let value = v end) in C.update

let mean = Mean.update
