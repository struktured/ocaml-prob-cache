module Running = Prob_cache_running
module Update_rules = Prob_cache_update_rules

module type DATA =
sig
(** Compute running statitics using recurrence equations. *)
type t = Running.t = { size : int         (** Number of observations. *)
         ; last : float       (** Last observation. *)
         ; max : float        (** Maxiumum. *)
         ; min : float        (** Minimum. *)
         ; sum : float        (** Sum . *)
         ; sum_sq : float     (** Sum of squares. *)
         ; mean : float       (** Mean. *)
         ; var : float        (** _Unbiased_ variance *)
} [@@deriving show]
end

module type S  =
sig
  type 'a update_rule = 'a Update_rules.Update_fn.t
  include DATA
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
  val update : cnt:int -> exp:float -> update_rule:'a update_rule
    -> ?prior_count:('a -> int) -> ?prior_exp:('a -> float) -> 'a -> t option -> t
  val join : ?obs:'a -> update_rule:'a update_rule -> t -> t -> t
  val part : ?obs:'a -> update_rule:'a update_rule -> t -> t -> t
  val empty : t
  val of_option : t option -> t
  end

module Make(Data:DATA) : S with type t = Data.t

module Predicate :
sig
  module type S =
  sig
    module Data : S
    type t
    val apply : t -> Data.t -> bool
  end
end
