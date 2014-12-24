(* A toy event model where it can be raining, a water sprinkler may be on, and the ground may be wet 
 * due to one, both, or none of these events *)
module Event = struct type t = IS_RAINING [@key 1] | SPRINKLER_ON [@key 2] | GROUND_IS_WET [@key 3] [@@deriving show, protobuf] end
module Model = Prob_cache_riak.Set_model.Make(Event)

open Event
let raining = Model.Events.of_list [IS_RAINING]
let sprinkler_on = Model.Events.of_list [SPRINKLER_ON]
let ground_wet_raining = Model.Events.of_list [IS_RAINING; GROUND_IS_WET]
let ground_wet_sprinkler_on = Model.Events.of_list [SPRINKLER_ON; GROUND_IS_WET]
let ground_wet = Model.Events.of_list [GROUND_IS_WET] 
let ground_not_wet_sprinkler_on = Model.Events.of_list [SPRINKLER_ON] 


open Core.Std
open Async.Std

let run ?(host="localhost") ?(port=8087) ?(name="toy-model") () =
let open Deferred.Result.Monad_infix in 
Model.with_model ~host ~port ~name 
  ( fun m -> Model.observe ground_wet_raining m >>=
    fun m -> Model.observe ground_wet_sprinkler_on m >>=
    fun m -> Model.observe ground_wet m >>=
    fun m -> Model.observe ground_not_wet_sprinkler_on m >>=
    fun m -> Model.prob ~cond:raining ground_wet m (* a = 1. *) >>=
    fun a -> Model.prob ~cond:sprinkler_on ground_wet m (* b = 5. *) >>=
    fun b -> Model.prob ground_wet m (* c = .75 *) >>|
    fun c -> 
    Print.print_string 
      ("P(GroundWet|Raining): " ^ Float.to_string a ^ "\n" ^
      "P(GroundWet|SprinklerOn): " ^ Float.to_string b ^ "\n" ^
      "P(GroundWet): " ^ Float.to_string c ^ "\n"); shutdown 0)

let () = 
  let host = try Some Sys.argv.(1) with _ -> None in
  let port = try Some (int_of_string Sys.argv.(2)) with _ -> None in
  let name = try Some Sys.argv.(3) with _ -> None in
  ignore(run ?host ?port ?name ());
  never_returns (Scheduler.go())

