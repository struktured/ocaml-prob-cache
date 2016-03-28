module Float = CCFloat
module RB = CCRingBuffer
module Opt = CCOpt

module type WEIGHT = sig val value : float end
module type OBS = sig type t end

module type OBS_WEIGHT = sig include OBS include WEIGHT end

module Online = Oml.Online

type update = Online.update

module UPDATE_FN(Obs:OBS) = struct
  type t = ?orig:float -> obs:Obs.t -> (** <-- parameters specific to ocaml prob cache *)
    size:float -> n_sum:float -> n_sum_sq:float ->
    n_size:float -> Online.t -> (** <-- parameters specific to oml.online *)
    exp:float -> (** <-- parameters specific to ocaml prob cache, since it's labeled *) update
end

module type Update_fn = sig
  module Obs : OBS
  val apply : UPDATE_FN(Obs).t (*?orig:float -> obs:Obs.t -> (** <-- parameters specific to ocaml prob cache *)
    size:float -> n_sum:float -> n_sum_sq:float ->
    n_size:float -> Online.t -> (** <-- parameters specific to oml.online *)
    exp:float -> (** <-- parameters specific to ocaml prob cache, since it's labeled *) update *)
end

module type Oml_update_rules = Online.Update_rules
  (*type t
  (* size = cnt??, n_sum = not defined, n_sum_sq = not_defined, n_size=not defined,
   * t = not defined, float=exp, update=update *)
  val apply : size:float -> n_sum:float -> n_sum_sq:float -> n_size:float ->
              t -> float -> update
end *)

module type RULE_WRAP =
sig
  module Obs : OBS
  include Online.Update_rules
  val add_obs : Obs.t -> unit
end

module Rule_wrap(Update_fn:Update_fn) : RULE_WRAP with module Obs = Update_fn.Obs =
  struct
  module Obs = Update_fn.Obs
  let rb_size = 2
  module Obs_buf = RB.Make(Obs)
  type t = Online.t
  type 'a state = {obs_buf: Obs_buf.t}
  let state = {obs_buf=Obs_buf.create ~bounded:true rb_size}
  let apply ~size ~n_sum ~n_sum_sq ~n_size t exp =
    let obs = Opt.get_exn @@ Obs_buf.take_front state.obs_buf in
    Update_fn.apply ?orig:None ~obs ~size ~n_sum ~n_sum_sq ~n_size t ~exp
  let add_obs (obs:Obs.t) = Obs_buf.push_back state.obs_buf obs
end

module type S = Update_fn

module type WEIGHT_PROVIDER =
sig
  module Obs : OBS
  val weight : UPDATE_FN(Obs).t
end

module Mean_weight_provider(Obs:OBS) : WEIGHT_PROVIDER with module Obs = Obs =
struct
  module Obs = Obs
  let weight ?orig ~(obs:Obs.t)
    ~(size:float) ~(n_sum:float) ~(n_sum_sq:float) ~(n_size:float) (t:Online.t)
    ~(exp:float) = Online.{n_mean=1.0 /. size;n_var=0.0}
end

module Constant_weight_provider (Obs:OBS) (Weight:WEIGHT) :
  WEIGHT_PROVIDER with module Obs = Obs =
struct
  module Obs = Obs
  let weight ?orig ~(obs:Obs.t)
    ~(size:float) ~(n_sum:float) ~(n_sum_sq:float) ~(n_size:float) (t:Online.t)
    ~(exp:float) = Online.{n_mean=Weight.value;n_var=0.0}
end

module Make_weighted(Weight_provider:WEIGHT_PROVIDER) =
struct
  module Obs = Weight_provider.Obs
  module Weight_provider = Weight_provider
  let apply ?(orig=0.) ~(obs:Obs.t)
    ~(size:float) ~(n_sum:float) ~(n_sum_sq:float) ~(n_size:float) (t:Online.t)
    ~(exp:float) = let open Online in
    if Float.equal 0.0 size then {n_mean=exp;n_var=0.0} else
      let online = Weight_provider.weight ~orig ~obs ~size ~exp ~n_sum ~n_sum_sq ~n_size t in
      {n_mean=orig +. (exp -. orig) *. online.n_mean; n_var=0.0}
end

module Constant(Obs:OBS)(Weight:WEIGHT) = Make_weighted(Constant_weight_provider(Obs)(Weight))
module Mean(Obs:OBS) = Make_weighted(Mean_weight_provider(Obs))
