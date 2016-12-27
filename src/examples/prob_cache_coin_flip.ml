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

let run() =
  let open Model in
  let open Model.Or_error.Monad_infix in
  Model.with_cache ?opt:None @@ fun (m:t) ->
  Model.observe events m >>| fun m ->
  Model.count events m >>| fun cnt ->
  Model.exp events m >>| fun exp ->
  Model.prob heads m >>| fun a -> (* a = 1. *)
  Model.prob tails m >>| fun b -> (* b = 0. *)
  Model.prob heads_tails m >>| fun c -> (* c = 1. *)
  print_endline ("P(HEADS) = " ^ (CCFloat.to_string a));
  print_endline ("P(TAILS) = " ^ (CCFloat.to_string b));
  print_endline ("P([HEADS, TAILS]) = " ^ (CCFloat.to_string c));
  Model.Or_error.return ()
end
