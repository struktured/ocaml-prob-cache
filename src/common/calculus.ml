open Events_common

module Make(Model:Model_decorator.S) = struct
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

  let or_error ?(prob=1.0) data = Result.map data
    ~f:(fun data -> {prob;data})
  let create ?(prob=1.0) ~data = {prob;data}
  let data t = t.data
  let prob t = t.prob
  let prob_or ?(value=0.) t = CCOpt.maybe prob value t
  let join ?obs ~update_rule t t' =
    Data.join ?obs ~update_rule (data t) (data t') |>
   fun data -> create ~prob:(prob t +. prob t') ~data 

  let empty = create ~prob:0. ~data:Data.empty
end

module M = Model
module Or_error = M.Or_error

module Literal =
  struct
    open Or_error.Monad_infix
    include Events.Event
    open Events.Infix

    let data m t = t $ (fun events -> M.data events m)
    let count m t = data m t >>| Data.count
    let derived ?prob m t = data m t >>| fun data -> Derived.create ?prob ~data
  end

module And = 
  struct
    open Literal
    open Or_error.Monad_infix
    include Events 

    let data m t = M.data t m 
    let count m t = data m t >>| Model.Data.count
    let derived ?prob m t = data m t >>| fun data -> Derived.create ?prob ~data

  end

module Or = 
  struct
    open Literal
    open And
    open Or_error.Monad_infix
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
    open Or_error.Monad_infix
    open Events.Infix

   
    (** [E|P]((A * B) + (B * C) || C+D) where lhs of 1st is lhs of given, 2nd is rhs *)
    type t = Or.t * Or.t [@@deriving show]
    let empty : t = (Or.empty, Or.empty)
        
    let create x given_y = (x, given_y)
    let none o : t = (o, Or.empty)

    (* TODO *)
    let given_count m y = Or_error.return 0
    let events_of_ors ors = Events.of_list []

    let rec breakdown_ors m y = function
        | [] -> Or_error.return 1.0
        | [x] -> And.count m (x & (events_of_ors y)) >>= 
          fun numer -> given_count m y >>| fun denom ->
          float numer /. float denom
        | x::xs ->
          breakdown_ors m y [x] >>= fun x_given_y ->
          breakdown_ors m y xs >>= fun xs_given_y ->
          CCList.fold_right Events.join xs Events.empty |> fun all_events ->
          breakdown_ors m y [all_events] >>| fun all_given_y ->
          x_given_y +. xs_given_y -. all_given_y
    let prob m : t -> Derived.t Or_error.t = 
      fun (ors, given) -> 
        Or.derived m given >>| fun derived_given -> derived_given
  end

module Infix =
struct
  let (&) = Events.Infix.(&)
  let (+) x y = Or.to_list y |> List.append (Or.to_list x)
  let (||) = Given.create
end
end


