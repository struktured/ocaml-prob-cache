open Prob_cache.Std
(* A toy event model where it can be raining, a water sprinkler may be on, and the ground may be wet 
 * due to one, both, or none of these events *)

module Event =
struct
  module type S =
  sig
    type t = IS_RAINING | SPRINKLER_ON | GROUND_IS_WET [@@deriving show, ord]
  end
end

module Make(Event:Event.S)(Model : Model.S with module Events.Event = Event) =
struct
open Event
let raining = Model.Events.of_list [IS_RAINING]
let sprinkler_on = Model.Events.of_list [SPRINKLER_ON]
let ground_wet_raining = Model.Events.of_list [IS_RAINING; GROUND_IS_WET]
let ground_wet_sprinkler_on = Model.Events.of_list [SPRINKLER_ON; GROUND_IS_WET]
let ground_wet = Model.Events.of_list [GROUND_IS_WET]
let ground_not_wet_sprinkler_on = Model.Events.of_list [SPRINKLER_ON]

open Model.Or_error.Monad_infix
let run () = Model.with_cache ?opt:None @@ fun m ->
  Model.observe ground_wet_raining m >>| fun m ->
  Model.observe ground_wet_sprinkler_on m >>| fun m ->
  Model.observe ground_wet m >>| fun m ->
  Model.observe ground_not_wet_sprinkler_on m >>| fun m ->
  Model.prob ~cond:raining ground_wet m (* a = 1 *) >>|
  fun ground_wet_given_raining ->
  Model.prob ~cond:sprinkler_on ground_wet m (* b = .5 *) >>|
  fun ground_wet_given_sprinkler_on ->
  Model.prob ground_wet m (* b = .75 *) >>| fun ground_wet ->
    print_endline ("P(GroundWet|Raining) = " ^ (CCFloat.to_string ground_wet_given_raining));
    print_endline ("P(GroundWet|SprinklerOn) = " ^ (CCFloat.to_string ground_wet_given_sprinkler_on));
    print_endline ("P(GroundWet) = " ^ (CCFloat.to_string ground_wet))
end
