(* A toy event model where it can be raining, a water sprinkler may be on, and the ground may be wet 
 * due to one, both, or none of these events *)
module Event = struct type t = IS_RAINING [@key 1] | SPRINKLER_ON [@key 2] | GROUND_IS_WET [@key 3] [@@deriving show, protobuf, ord] end
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

let rand = string_of_int (CCRandom.run (CCRandom.int 100000))
let run ?(host="localhost") ?(port=8087) ?(name="toy-model-"^rand) () =
let open Deferred.Result.Monad_infix in 
Model.with_model ~host ~port ~name 
  (fun m -> Model.observe ground_wet_raining m >>=
    Model.observe ground_wet_sprinkler_on >>=
    Model.observe ground_wet >>=
    Model.observe ground_not_wet_sprinkler_on >>=
    fun m -> Model.prob ~cond:raining ground_wet m (* returns 1. *) >>=
    fun p_ground_wet_given_raining -> Model.prob ~cond:sprinkler_on ground_wet m (* returns .5 *) >>=
    fun p_ground_wet_given_sprinkler_on -> Model.prob ground_wet m (* returns .75 *) >>|
    fun p_ground_wet -> 
    Print.print_string 
      ("P(GroundWet|Raining) = " ^ Float.to_string p_ground_wet_given_raining ^ "\n" ^
       "P(GroundWet|SprinklerOn) = " ^ Float.to_string p_ground_wet_given_sprinkler_on ^ "\n" ^
       "P(GroundWet) = " ^ Float.to_string p_ground_wet ^ "\n"); shutdown 0)

let () = 
  let host = try Some Sys.argv.(1) with _ -> None in
  let port = try Some (int_of_string Sys.argv.(2)) with _ -> None in
  let name = try Some Sys.argv.(3) with _ -> None in
  ignore(run ?host ?port ?name ());
  never_returns (Scheduler.go())

