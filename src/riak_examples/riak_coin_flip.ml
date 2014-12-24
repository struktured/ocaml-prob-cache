(* Models a sequence of coin flips, not necesssarily where each flip is independent *) 
module Coin = struct type t = HEADS [@key 1] | TAILS [@key 2] [@@deriving show, protobuf] end
module Model = Prob_cache_riak.Sequence_model.Make(Coin)

open Coin
let events = Model.Events.of_list [HEADS;TAILS;HEADS;TAILS] 
let heads = Model.Events.of_list [HEADS] 
let tails = Model.Events.of_list [TAILS] 
let heads_tails = Model.Events.of_list [HEADS;TAILS] 



open Core.Std
open Async.Std

let run ?(host="localhost") ?(port=8087) ?(name="coin-flips") () =
let open Deferred.Result.Monad_infix in 
Model.with_model ~host ~port ~name
  (fun m -> 
    Model.observe events m >>=
    fun m -> Model.prob heads m (* a = 1. *) >>=
    fun a -> Model.prob tails m (* b = 0. *) >>=
    fun b -> Model.prob heads_tails m (* c = 1. *) >>|
    fun c -> 
    Print.print_string 
      ("P(HEADS) = " ^ Float.to_string a ^ ",\n" ^
      "P(TAILS) =  " ^ Float.to_string b ^ "\n" ^
      "P([HEADS,TAILS]) = " ^ Float.to_string c ^ "\n");shutdown 0)

let () = 
  let host = try Some Sys.argv.(1) with _ -> None in
  let port = try Some (int_of_string Sys.argv.(2)) with _ -> None in
  let name = try Some Sys.argv.(3) with _ -> None in
  ignore(run ?host ?port ?name ());
  never_returns (Scheduler.go())

