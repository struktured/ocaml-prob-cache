

module type DATA = 
sig
  type t = {cnt:int; exp:float} [@@deriving show]
end

module type S =
sig
  module T : DATA
  type t = T.t
  val create : cnt:int -> exp:float -> t
  val count : t -> int
  val expect : t -> float
  val update : cnt:int -> exp:float -> update_rule:Update_rules.Update_fn.t 
    -> prior_count:('a -> int) -> prior_exp:('a -> float) -> 'a -> t option -> t
end

module Make(Data:DATA) : S with module T = Data
