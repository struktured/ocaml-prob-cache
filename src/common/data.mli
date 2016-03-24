
module type OBS = Update_rules.OBS
module type DATA =
sig
(** Compute running statitics using recurrence equations. *)
type t = Oml.Online.t = { size : int         (** Number of observations. *)
         ; last : float       (** Last observation. *)
         ; max : float        (** Maxiumum. *)
         ; min : float        (** Minimum. *)
         ; sum : float        (** Sum . *)
         ; sum_sq : float     (** Sum of squares. *)
         ; mean : float       (** Mean. *)
         ; var : float        (** _Unbiased_ variance *)
} [@@deriving show]
end

module type S =
sig
  module Obs : Update_rules.OBS
  module T : DATA
  type t = T.t [@@deriving show]
  val create : cnt:int -> exp:float -> t
  val bootstrap : cnt:int -> ?last:float -> ?max:float -> ?min:float ->
    sum:float -> sum_sq:float -> ?mean:float -> ?var:float ->
    ?stddev:float -> unit -> t
  val count : t -> int
  val expect : t -> float
  val var : t -> float
  val max : t -> float
  val min : t -> float
  val sum : t -> float
  val last : t -> float
  val sum_sq : t -> float
  val update : cnt:int -> exp:float -> ?prior_count:(Obs.t -> int) ->
    ?prior_exp:(Obs.t -> float) -> Obs.t -> t option -> t
  val join : obs:Obs.t -> t -> t -> t
end

module Make(Update_rule:Update_rules.Update_fn) (Data:DATA) :
  S with module T = Data and module Obs = Update_rule.Obs

module Make_with_defaults(Obs:OBS)(Data:DATA) :
  S with module T = Data and module Obs = Obs
