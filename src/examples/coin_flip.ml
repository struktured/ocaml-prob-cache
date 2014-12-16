(* Models a sequence of coin flips, not necesssarily where each flip is independent *) 
module Coin = struct type t = HEADS | TAILS [@@deriving show, ord] end
module Model = Prob_cache_containers.Sequence_model.Make(Coin)

open Coin
let events = Model.Events.of_list [HEADS;TAILS;HEADS;TAILS] 
let heads = Model.Events.of_list [HEADS] 
let tails = Model.Events.of_list [TAILS] 
let heads_tails = Model.Events.of_list [HEADS;TAILS] 

let m = Model.create "coin-flips" |>
  fun m -> Model.observe events m
  
let cnt,exp = Model.count events m, Model.exp events m (* cnt=1, exp = 1. *)

let a = Model.prob heads m (* a = 1. *)
let b = Model.prob tails m (* b = 0. *)
let c = Model.prob heads_tails m (* c = 1. *)

