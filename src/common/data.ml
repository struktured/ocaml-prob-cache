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

module type S =
sig
  module T : DATA
  type t =  T.t
  val create : cnt:int -> exp:float -> t
  val count : t -> int
  val expect : t -> float
  val var : t -> float
  val max : t -> float
  val min : t -> float
  val sum : t -> float
  val last : t -> float
  val update : cnt:int -> exp:float -> update_rule:'a Update_rules.Update_fn.t
    -> ?prior_count:('a -> int) -> ?prior_exp:('a -> float) ->
      'a -> t option -> t
end


module Make(Data:DATA) =
struct
  module T = Data
  type t = Data.t
  open T
  let create ~cnt ~exp =
   Oml.Running.init ~size:cnt exp
  let count t = t.Oml.Running.size
  let expect t = t.Oml.Running.mean
  let var t = t.Oml.Running.var
  let min t = t.Oml.Running.min
  let max t = t.Oml.Running.max
  let sum t = t.Oml.Running.sum
  let last t = t.Oml.Running.last

  let update ~cnt ~exp ~(update_rule:'a Update_rules.Update_fn.t)
    ?prior_count ?prior_exp (obs:'a) t_opt =
    let prior_count = CCOpt.get_lazy (fun () -> fun _ -> 0) prior_count in
    let prior_exp = CCOpt.get_lazy (fun () -> fun _ -> 0.0) prior_exp in
    let t = CCOpt.get_lazy (fun () ->
      create ~cnt:(prior_count obs) ~exp:(prior_exp obs)) t_opt in
    let mean_update ~size ~n_sum ~n_sum_sq ~n_size t v =
      update_rule ~obs ~exp:v ~cnt:(int_of_float n_size) ~orig:(expect t) in
    Oml.Running.update ~mean_update ~size:cnt t exp
end
