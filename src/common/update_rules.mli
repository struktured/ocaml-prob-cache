module type WEIGHT = sig val value : float end
module type OBS = sig type t end

module type OBS_WEIGHT = sig include OBS include WEIGHT end

module Online : module type of Oml.Online

type update = Online.update

module UPDATE_FN: functor (Obs : OBS) -> sig
  type t = ?orig:float -> obs:Obs.t -> (** <-- parameters specific to ocaml prob cache *)
    size:float -> n_sum:float -> n_sum_sq:float ->
    n_size:float -> Online.t -> (** <-- parameters specific to oml.online *)
    exp:float -> update
end


module type Update_fn = sig
  module Obs : OBS
  val apply : UPDATE_FN(Obs).t
end

module type S = Update_fn

module type WEIGHT_PROVIDER =
sig
  module Obs : OBS
  val weight : UPDATE_FN(Obs).t
end


module Make_weighted : functor(Weight_provider:WEIGHT_PROVIDER) -> S

module Constant : functor(Obs:OBS)(Weight:WEIGHT) -> S

module Mean : functor(Obs:OBS) -> S
