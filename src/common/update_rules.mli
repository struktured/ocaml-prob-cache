module type WEIGHT = sig val value : float end
module type OBS = sig type t end

module type OBS_WEIGHT = sig include OBS include WEIGHT end

module UPDATE_FN: functor (Obs : OBS) -> sig
  type t = ?orig:float -> obs:Obs.t -> (** <-- parameters specific to ocaml prob cache *)
    size:float -> n_sum:float -> n_sum_sq:float ->
    n_size:float -> Oml.Online.t -> (** <-- parameters specific to Oml.Online *)
    exp:float -> Oml.Online.update
end

type 'a update_fn = ?orig:float -> obs:'a -> (** <-- parameters specific to ocaml prob cache *)
    size:float -> n_sum:float -> n_sum_sq:float ->
    n_size:float -> Oml.Online.t -> (** <-- parameters specific to oml.online *)
    exp:float -> (** <-- parameters specific to ocaml prob cache, since it's labeled *) Oml.Online.update

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

module Make_weighted : functor(Weight_provider:WEIGHT_PROVIDER) ->
  S with module Obs = Weight_provider.Obs

module Constant : functor(Obs:OBS)(Weight:WEIGHT) -> S with module Obs = Obs

module Mean : functor(Obs:OBS) -> S with module Obs = Obs

module type RULE_WRAP =
sig
  module Obs : OBS
  include Oml.Online.Update_rules
  val add_obs : Obs.t -> unit
end

module Rule_wrap :
  functor(Update_fn:Update_fn) -> RULE_WRAP with module Obs = Update_fn.Obs
