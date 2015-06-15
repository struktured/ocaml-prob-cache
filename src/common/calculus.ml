open Events_common

module Make(Model:Model_common.S) = struct
module Events = Model.Events
module Data = Model.Data
module Result = Model.Result
module Float = CCFloat


(* Counting occurences of events given function N(A & B & .. & Z) = number of times events {A,B,..,Z} jointly occured
 * How often does A occur? Occur(A) = N(A)
 * How often does either A or B occur? Occur(A,B) = Occur(A) + Occur(B) - Occur(A & B)
 * How often does either (A & B) or (B & C) occur ? N(A & B) + N(B & C) - N(A & B & C) equiv to Occur(A & B, B & C)
 * How often does either A or B or C occur? N(A) + N(B) + N(C) - N(A & B) - N(B & C) - N(A & C) + N(A & B & C)
    Occur(A, B, C) =
      Occur(A) + Occur(B,C) - Occur(A & B, A & C)
    Occur(A, B, C, D) =
      Occur(A) + Occur(B,C,D) - Occur(A & B, A & C, A & D)
      
      Occur(A) + Occur(B) + Occur(C) -
    Occur(A,B) - Occur(B,C) - Occur(A,C) + Occur(A&B, C) + Occur(A & C, B) + Occur(B & C, A)
 * How often does either A or B or C or D occur?
     N(A) + N(B) + N(C) + N(D) - N(A & B) - N(B & C) - N(A & C) - N(A & D) - N(C & D) + N(A & B & C) + N(A & C & D) + N(B & C & D)
     - N(A & B & C & D)
 * How often does A & B occur N(A & B), how often does A occur without B? N(A) - N(A & B)
 *
 *
 *Cond(A, B, C) =
      Cond(A,C) + Cond(B,C) - Cond(A & B, A & C)
 *)

(**
   * Basic Composition:
    - Joint Expectation / Probability (AND) :
      [E|P](A & B) = expectation/probability of A and B jointly occuring.
    - Sum Expectation / Probability (OR) :
      E(A + B) = [N(A)*E(A) + N(B)*E(B) - N(A & B)*E(A & B)] / [N(A) + N(B) - N(A & B)] =
      P(A + B) = [N(A) + N(B)] / N
      [E|P](A + (B & C)) = [E|P](A) + [E|P](B & C)
  
   * !!Not supported!!: 
     [E|P](A & (B+C)) (eg. ANDs of ORs)
     [E|P](~ A) = [E|P](B + C + D) = Expectation / Probability of sum complement (NOT)
     [E|P](~ (A + B)) = [E|P](C + D)
     [E|P](~ (A & B)) = [E|P](C & D)

   * Conditionals:
     P(A & B) = N(A & B) / N([])
     P(A | B) = N(A & B) / N(B)
     P(A | B & C) = N(A & B & C) / N(B & C)
     P(A + B | C) = P(A|C) + P(B|C) - P(A & B|C) 
     P(A + B + C|D) = P(A|D) + P(B + C|D) - P(A & B & C|D)
     P(A | B + C) = [N(A & B) + N(A & C) - N(A & B & C)] / N(B+C)
     P(A | B + C + D) = [N(A & B) + N(A & C) + N(A & D) - N(A & B & C & D)] / N(B+C+D)

*)

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

module M = Model.Or_errors
module Or_error = Model.Or_error

module Literal =
  struct
    open Or_error.Infix
    include Events.Event
    open Events.Infix

    let data m t = t $ (fun events -> M.data events m)
    let count m t = data m t >>| Data.count
    let derived ?prob m t = data m t >>| fun data -> Derived.create ?prob ~data
  end

module And = 
  struct
    open Literal
    open Or_error.Infix
    include Events 

    let data m t = M.data t m 
    let count m t = data m t >>| Model.Data.count
    let derived ?prob m t = data m t >>| fun data -> Derived.create ?prob ~data

  end

module Or = 
  struct
    open Literal
    open And
    open Or_error.Infix
    type t = And.t list [@@deriving show]

    open Events.Infix
    let of_list t = t
    let to_list t = t
    let empty = []
    let rec fold ~join ~part f m empty : t -> 'a Or_error.t = function
      | [] -> Or_error.return empty
      | x::xs ->
          f m x >>= fun x_data ->
          fold ~join ~part f m empty xs >>= fun xs_data ->
          List.map (fun events -> (x & events) |> 
            fun e' -> f m e' >>| fun d -> (e', d)) xs
          |> Or_error.all >>| 
          begin fun datas -> 
            CCList.fold_right (fun (obs, d) -> join ?obs:(Some obs) d) datas empty
          end >>|
          fun x_and_xs_data -> join ?obs:None x_data xs_data |>
          fun joined -> part ?obs:None joined x_and_xs_data

    let data m : t -> Data.t Or_error.t =
      let update_rule = Model.update_rule m in 
      let join = Data.join ~update_rule in
      let part = Data.part ~update_rule in
      let empty = Data.empty in
      fold ~join ~part And.data m empty

    let count m : t -> int Or_error.t = 
      let join ?obs t t' = t+t' in
      let part ?obs t t' = t-t' in
      let empty = 0 in
      fold ~join ~part And.count m empty

    let derived ?prob m t = 
      data m t >>| fun data -> Derived.create ?prob ~data
  
  end

module Given =
  struct
    open Literal
    open And
    open Or
    open Or_error.Infix
    open Events.Infix

   
    (** [E|P]((A * B) + (B * C) || C+D) where lhs of 1st is lhs of given, 2nd is rhs *)
    type t = Or.t * Or.t [@@deriving show]
    let empty : t = (Or.empty, Or.empty)

    let none o : t = (o, Or.empty)
     

    let rec breakdown_ors m y (ors:And.t list) = function 
        | [] -> Or_error.return 1.0
        | [x::xs] -> 
            let x_given_y = breakdown_ors m xs
    let prob m : t -> Derived.t Or_error.t = 
      fun (ors, given) -> 
        
        Or.derived m or2 >>| fun derived_or2 -> derived_or2

      (*        P(A + B + C|D) = P(A|D) + P(B + C|D) - P(A & B & C|D)*)

  end

(*
type t =
  | Events of Event.t
  | And of t list
  | Or of t list
  | Not of Events.t
  | Given of t * t [@@deriving show]


let _and t t' = And [t;t']
let _or t t' = Or [t;t']
let _not t = Not t
let _given t t' = Given (t, t')
*)

(*
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
*)

let rec eval (m:Model.t) (t:t) =
  let open Model.Or_error.Infix in
  let module M = Model.Or_errors in
  match t with
  | [] -> Derived.or_error (M.data e m)
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
  let (||) =_given
end
end


