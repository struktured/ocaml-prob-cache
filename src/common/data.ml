

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
  val update : cnt:int -> exp:float -> update_rule:'a Update_rules.Update_fn.t 
    -> prior_count:('a -> int) -> prior_exp:('a -> float) -> 'a -> t option -> t
end


module Make(Data:DATA) = 
struct
  module T = Data
  type t = Data.t
  open T
  let create ~cnt ~exp = {cnt;exp}
  let count t = t.cnt
  let expect t = t.exp

  let update ~cnt ~exp ~(update_rule:'a Update_rules.Update_fn.t) ~(prior_count:'a -> int) 
    ~(prior_exp:'a -> float) (obs:'a) t_opt = 
    let t = CCOpt.get_lazy (fun () -> 
      create ~cnt:(prior_count obs) ~exp:(prior_exp obs)) t_opt in
    let cnt = t.cnt + cnt in
    create ~cnt ~exp:(update_rule ~obs ~exp ~cnt ~orig:t.exp)
end
