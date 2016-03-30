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
  val update : cnt:int -> exp:float -> 
    ?update_rule:(Obs.t Update_rules.update_fn)-> ?prior_count:(Obs.t -> int) ->
    ?prior_exp:(Obs.t -> float) -> Obs.t -> t option -> t
  val join : obs:Obs.t -> t -> t -> t
end


module Make(Obs:OBS)(Data:DATA) =
struct
  module T = Data
  module Obs = Obs
  type t = Data.t [@@deriving show]
  open T
  let create ~cnt ~exp =
   Oml.Online.init ~size:cnt exp

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

  let count t = t.Oml.Online.size
  let expect t = t.Oml.Online.mean
  let var t = t.Oml.Online.var
  let min t = t.Oml.Online.min
  let max t = t.Oml.Online.max
  let sum t = t.Oml.Online.sum
  let last t = t.Oml.Online.last
  let sum_sq t = t.Oml.Online.sum_sq

  let _mean_update ~update_rule
    ~obs ~size ~n_sum ~n_sum_sq ~n_size t v = debug @@
      Printf.sprintf "t=\"%s\", size=%d, n_sum=%f, n_sum_sqr=%f, n_size=%f, v=%f" 
        (Data.show t) size n_sum n_sum_sq n_size v;
      update_rule ~obs ~exp:v ~cnt:(int_of_float n_size) ~orig:(expect t)

    let update ~cnt ~exp ?update_rule ?prior_count ?prior_exp obs t_opt =
      let update_fn = Opt.get_lazy
        (fun () -> let module Mean = Update_rules.Mean(Obs) in Mean.apply) update_rule in
      let module Update_fn = struct module Obs = Obs let apply = update_fn end in
      let module Wrapped = Update_rules.Rule_wrap(Update_fn) in
      let module Update = Oml.Online.Make(Wrapped) in
      Wrapped.add_obs obs;
      let prior_count = Opt.get_lazy (fun () -> Fun.const 0) prior_count in
      let prior_exp = Opt.get_lazy (fun () -> Fun.const 0.0) prior_exp in
      let t = Opt.get_lazy (fun () -> create ~cnt:(prior_count obs)
          ~exp:(prior_exp obs)) t_opt in
      Update.update ~size:cnt t exp
  let join ~obs (t:t) (t':t) = Oml.Online.join t t'
end
