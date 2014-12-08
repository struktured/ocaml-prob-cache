type t = {cnt:int; exp:float} [@@deriving show]

let create ~cnt ~exp = {cnt;exp}
let count t = t.cnt
let expect t = t.exp

let update ~cnt ~exp ~(update_rule:Update_rules.Update_fn.t) ~(prior_count:'a -> int) ~(prior_exp:'a -> float) (obs:'a) t_opt = 
  let t = CCOpt.get_lazy (fun () -> 
    create ~cnt:(prior_count obs) ~exp:(prior_exp obs)) t_opt in
  let cnt = t.cnt + cnt in
  create ~cnt ~exp:(update_rule ~obs:exp ~cnt ~orig:t.exp)

