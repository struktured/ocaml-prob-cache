open Prob_cache.Std
(* Models a sequence of coin flips, not necesssarily where each flip is independent *)
module Coin =
struct
module type S =
sig
type t = HEADS | TAILS [@@deriving show, ord]
end
end

module Make(Coin:Coin.S)(Sequence_model : Model.S with module Events.Event = Coin) =
struct
module Model = Sequence_model
open Coin
let events = Model.Events.of_list [HEADS;TAILS;HEADS;TAILS]
let heads = Model.Events.of_list [HEADS]
let tails = Model.Events.of_list [TAILS]
let heads_tails = Model.Events.of_list [HEADS;TAILS]

let go() =
let open Model.Or_error.Monad_infix in
Model.create "coin-flips" >>| fun m ->
Model.observe events cnt,exp = Model.count events m, Model.exp events m (* cnt=1, exp = 1. *)
let cnt,exp = Model.count events m, Model.exp events m (* cnt=1, exp = 1. *)

let a = Model.prob heads m (* a = 1. *)
let b = Model.prob tails m (* b = 0. *)
let c = Model.prob heads_tails m (* c = 1. *)

let () =
  print_endline ("P(HEADS) = " ^ (CCFloat.to_string a));
  print_endline ("P(TAILS) = " ^ (CCFloat.to_string b));
  print_endline ("P([HEADS, TAILS]) = " ^ (CCFloat.to_string c))
end
