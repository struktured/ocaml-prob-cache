type t = {cnt:int; exp:float} [@@deriving show]

let count t = t.cnt
let expect t = t.exp

let update ?(cnt=1) ?(exp=1.0) (t:t option) = CCOpt.get {cnt;exp} 
  (CCOpt.map (fun t -> {cnt=(count t) + cnt;exp=(expect t) +. exp}) t)


