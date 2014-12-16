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

let run ?(host="localhost") ?(port=8087) () = ()
(* WIP!!
Model.with_model ~host ~port ~name:"coin-flips" 
  (fun m -> 
    let result = Model.observe events m in
    let a = Model.prob heads m (* a = 1. *) in
    let b = Model.prob tails m (* b = 0. *) in
    let c = Model.prob heads_tails m (* c = 1. *) in
    Async.Std.Print.print_string 
      ("H: " ^ Float.to_string a ^ ", " ^
      "T: " ^ Float.to_string b ^ ", " ^
      "HT: " ^ Float.to_string c ^ "\n")
*)



