open Events_common

module Make(Model:Model_common.S) = struct
module Events = Model.Events
module Data = Model.Data
module Result = Model.Result
module Float = CCFloat

module Derived =
struct
  type t = {prob:float; data: Data.t} [@@deriving show]

  let or_error ?(prob=1.0) data = match data with 
    | Result.Ok data -> Result.Ok {prob;data}
    | Result.Error _ as e -> e
  let create ?(prob=1.0) ~data = {prob;data}
  let data t = t.data
  let prob t = t.prob
  let prob_or ?(value=0.) t = CCOpt.maybe prob value t
  let join ?obs ~update_rule t t' =
    Data.join ?obs ~update_rule (data t) (data t') |> 
   fun data -> create ~prob:(prob t +. prob t') ~data 

  let empty = create ~prob:0. ~data:Data.empty 
end

type t =
  | Events of Events.t
  | And of t list
  | Or of t list
  | Not of t
  | Given of t * t [@@deriving show]

let _and t t' = And [t;t']
let _or t t' = Or [t;t']
let _not t = Not t
let _given t t' = Given (t, t')

let rec expand m =
  let open Model.Or_error.Infix in
  let module M = Model.Or_errors in function
  | Events e -> Model.Or_error.return [e]
  | And t_list ->
    begin
      List.map (fun e -> expand m e) t_list
      |> Model.Or_error.all >>| List.flatten
    end
  | Not t ->
    begin match t with
  | Events e ->
    M.complement e m >>= fun complement -> expand m (Events complement)
  | Not t' -> expand m t'
  | And t_list -> List.map (fun t -> Not t) t_list
    |> fun e_list' -> expand m (And e_list')
  | Or t_list -> List.map (fun t -> Not t) t_list
    |> fun e_list' -> expand m (Or e_list')
  | Given (t1, t2) -> expand m (Given (Not t1, Not t2))
    end
  | Or t_list -> failwith("NYI")
(*    expand m t1 >>= fun e1 ->
    expand m t2 >>| fun e2 -> List.flatten [e1;e2]*)
  | Given (t,t') -> failwith("NYI")
(* ->
    expand m t1 >>= fun e1 ->
    expand m t2 >>| fun e2 -> List.flatten [e1;e2]*)

let rec eval (m:Model.t) (t:t) =
  let open Model.Or_error.Infix in
  let module M = Model.Or_errors in
  match t with
| Events e -> Derived.or_error (M.data e m)
| And t_list -> failwith("NYI")
| Or t_list -> 
    List.fold_left (fun d t -> d >>= fun d -> eval m t >>| fun d' ->
    Derived.join d d' ~update_rule:(Model.update_rule m))
      (Model.Or_error.return Derived.empty) t_list 
| Not t ->
    begin match t with
  | Not t' -> eval m t'
  | And e -> M.complement e m >>= fun complement -> eval m (And complement)
  | Or (e, e') -> eval m (Or (Not e, Not e'))
  | Given (t1, t2) -> eval m (Given (Not t1, Not t2))
    end
| Given (t1, t2) ->
    match t1,t2 with
    | And e1, And e2 ->
      let joined = Events.join e1 e2 in
      M.data joined m >>= fun d_joined ->
      M.data e2 m >>| fun d_cond ->
        let joined_events_count = Data.count d_joined in
        let cond_count = Data.count d_cond in
    (* (a) If the conditional probability is zero then is so must been the whole probability 
       (b) If the conditional (possibly empty) events have been observed then we normalize by its frequency count 
     *)
    let prob =  
      if (cond_count = 0) then (Float.of_int 0) else
    (Float.of_int joined_events_count) /. (Float.of_int cond_count) in
    Derived.create ~prob ~data:d_joined
    | t1, t2 ->(* eval m t1 -> fun d1 -> eval m t2 -> fun d2 ->
     let joined = Events.join e1 e2 in*)
     failwith("NYI")

module Infix =
struct
  let (&) = _and
  let (+) = _or
  let (!) = _not
  let (||) =_given
end
end


