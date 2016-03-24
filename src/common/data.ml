module Online = Oml.Online
module Opt = CCOpt
module type OBS = Update_rules.OBS
module Fun = CCFun
module type DATA =
sig
(** Compute running statitics using recurrence equations. *)
type t = Oml.Online.t = {
           size : int         (** Number of observations. *)
         ; last : float       (** Last observation. *)
         ; max : float        (** Maxiumum. *)
         ; min : float        (** Minimum. *)
         ; sum : float        (** Sum . *)
         ; sum_sq : float     (** Sum of squares. *)
         ; mean : float       (** Mean. *)
         ; var : float        (** _Unbiased_ variance *)
} [@@deriving show]
end

let debug = ref false
let debug s = match !debug with true -> print_endline @@ "[data] " ^ s | false -> ()

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


module Make(Update_fn:Update_rules.Update_fn) (Data:DATA) =
struct
  module Update_fn = Update_fn
  module Obs = Update_fn.Obs
  module T = Data
  type t = Data.t [@@deriving show]
  open T
  let create ~cnt ~exp =
   Online.init ~size:cnt exp

  let bootstrap ~cnt ?last ?max ?min
    ~sum ~sum_sq ?mean ?var ?stddev =
      let module O = Opt in Fun.const @@
        let cnt_f = float cnt in
        let mean = O.get_lazy (fun () -> sum /. float cnt) mean in
        let last = O.get mean last in
        let max = O.get last max in
        let min = O.get last min in
        let var = O.get_lazy (fun () -> O.get_lazy (fun () ->
          sum_sq -. (sum *. sum) /. cnt_f /. (cnt_f -. 1.0)) stddev) var in
        {size=cnt;last;mean;max;min;var;sum_sq;sum}

  let count t = t.Online.size
  let expect t = t.Online.mean
  let var t = t.Online.var
  let min t = t.Online.min
  let max t = t.Online.max
  let sum t = t.Online.sum
  let last t = t.Online.last
  let sum_sq t = t.Online.sum_sq

  let _mean_update ~update_rule
    ~obs ~size ~n_sum ~n_sum_sq ~n_size t v = debug @@ 
      Printf.sprintf "t=\"%s\", size=%d, n_sum=%f, n_sum_sqr=%f, n_size=%f, v=%f" 
        (Data.show t) size n_sum n_sum_sq n_size v;
      update_rule ~obs ~exp:v ~cnt:(int_of_float n_size) ~orig:(expect t)

  
(*
  module Make(Update_rules:Online.Update_rules) = struct
    module Online = Online.Make(Update_rules)
    let apply ~cnt ~exp ?prior_count ?prior_exp (obs:'a) t_opt =
    let prior_count = Opt.get_lazy (fun () -> Fun.const 0) prior_count in
    let prior_exp = Opt.get_lazy (fun () -> Fun.const 0.0) prior_exp in
    let t = Opt.get_lazy (fun () -> create ~cnt:(prior_count obs) 
      ~exp:(prior_exp obs)) t_opt in
    Online.update ~size:cnt t exp

   let join ~obs = Online.join
   end
*)

  let update ~cnt ~exp ?prior_count ?prior_exp obs t_opt = failwith("nyi")
  let join ~obs t t' = Online.join t t'
end

module Make_with_defaults(Obs:OBS)(Data:DATA) =
struct
  module Update_rule = Update_rules.Mean(Obs)
  include Make(Update_rule)(Data)
end


