(* A toy event model where it can be raining, a water sprinkler may be on, and the ground may be wet 
 * due to one, both, or none of these events *)
module Event = struct type t = IS_RAINING | SPRINKLER_ON | GROUND_IS_WET [@@deriving show, ord] end
module Model = Prob_cache_containers.Set_model.Make(Event)

open Event
let raining = Model.Events.of_list [IS_RAINING]
let sprinkler_on = Model.Events.of_list [SPRINKLER_ON]
let ground_wet_raining = Model.Events.of_list [IS_RAINING; GROUND_IS_WET]
let ground_wet_sprinkler_on = Model.Events.of_list [SPRINKLER_ON; GROUND_IS_WET]
let ground_wet = Model.Events.of_list [GROUND_IS_WET] 
let ground_not_wet_sprinkler_on = Model.Events.of_list [SPRINKLER_ON] 

let m = Model.create "toy-model" |>
  Model.observe ground_wet_raining  |>
  Model.observe ground_wet_sprinkler_on  |>
  Model.observe ground_wet  |>
  Model.observe ground_not_wet_sprinkler_on 
  
let ground_wet_given_raining = Model.prob ~cond:raining ground_wet m (* a = 1 *)
let ground_wet_given_sprinkler_on = Model.prob ~cond:sprinkler_on ground_wet m (* b = .5 *)
let ground_wet = Model.prob ground_wet m (* b = .75 *)

let () = 
  print_endline ("P(GroundWet|Raining) = " ^ (CCFloat.to_string ground_wet_given_raining));
  print_endline ("P(GroundWet|SprinklerOn) = " ^ (CCFloat.to_string ground_wet_given_sprinkler_on));
  print_endline ("P(GroundWet) = " ^ (CCFloat.to_string ground_wet))
