module type DATA =
sig
(** Compute running statitics using recurrence equations. *)
type t = Oml.Running.t = { size : int         (** Number of observations. *)
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
  type 'a update_rule = 'a Update_rules.Update_fn.t
  module T : DATA
  type t =  T.t [@@deriving show]
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
    -> ?prior_count:('a -> int) -> ?prior_exp:('a -> float) ->
      'a -> t option -> t
  val join : ?obs:'a -> update_rule:'a update_rule -> t -> t -> t
  val part : ?obs:'a -> update_rule:'a update_rule -> t -> t -> t
  val empty : t
  val of_option : t option -> t
end

module Fun = CCFun
module Make(Data:DATA) =
struct
  type 'a update_rule = 'a Update_rules.Update_fn.t
  module T = Data
  type t = Data.t [@@deriving show]
  open T
  let create ~cnt ~exp =
   Oml.Running.init ~size:cnt exp

  let bootstrap ~cnt ?last ?max ?min
    ~sum ~sum_sq ?mean ?var ?stddev =
      let module O = CCOpt in Fun.const @@
        let cnt_f = float cnt in
        let mean = O.get_lazy (fun () -> sum /. float cnt) mean in
        let last = O.get mean last in
        let max = O.get last max in
        let min = O.get last min in
        let var = O.get_lazy (fun () -> O.get_lazy (fun () ->
          sum_sq -. (sum *. sum) /. cnt_f /. (cnt_f -. 1.0)) stddev) var in
        {size=cnt;last;mean;max;min;var;sum_sq;sum}

  let count t = t.Oml.Running.size
  let expect t = t.Oml.Running.mean
  let var t = t.Oml.Running.var
  let min t = t.Oml.Running.min
  let max t = t.Oml.Running.max
  let sum t = t.Oml.Running.sum
  let last t = t.Oml.Running.last
  let sum_sq t = t.Oml.Running.sum_sq

  let _mean_update ~(update_rule:'a update_rule)
    ?obs ~size ~n_sum ~n_sum_sq ~n_size t v = debug @@ 
      Printf.sprintf "t=\"%s\", size=%d, n_sum=%f, n_sum_sqr=%f, n_size=%f, v=%f" 
        (Data.show t) size n_sum n_sum_sq n_size v;
      update_rule ?obs ~exp:v ~cnt:(int_of_float n_size) ~orig:(expect t)

  let update ~cnt ~exp ~(update_rule:'a update_rule)
    ?prior_count ?prior_exp (obs:'a) t_opt =
    let prior_count = CCOpt.get_lazy (fun () -> Fun.const 0) prior_count in
    let prior_exp = CCOpt.get_lazy (fun () -> Fun.const 0.0) prior_exp in
    let t = CCOpt.get_lazy (fun () -> create ~cnt:(prior_count obs) 
      ~exp:(prior_exp obs)) t_opt in
    let mean_update = _mean_update ~update_rule ~obs in
    Oml.Running.update ~mean_update ~size:cnt t exp

  let join ?obs ~update_rule =
    let mean_update = _mean_update ?obs ~update_rule in
    Oml.Running.join ~mean_update ?var_update:None

  let part ?obs ~update_rule = 
    let mean_update = _mean_update ?obs ~update_rule in
    Oml.Running.part ~mean_update ?var_update:None 

  let empty = Oml.Running.empty
  let of_option = function 
    | None -> empty
    | Some d -> d
 
(* let join_maybe ?obs ~update_rule t t' =
    function 
    | None,None -> None
    | Some t, Some t' -> Some (join ?obs ~update_rule t t')
    | None, Some t' -> Some t'
    | Some t, None -> Some t  *)
  end
