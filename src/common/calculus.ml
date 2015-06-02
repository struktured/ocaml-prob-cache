
(*module type EVENTS =
  sig
    type t [@@deriving show]
  end
*)

module type EVENT = sig type t end
module type EVENTS = sig module Event : EVENT type t end

module Make(Events:EVENTS) =
struct
type t = Events.t
let _and t t' = t 
let _or t t' = t 
let _not t = t
let _given t t' = t
let _in t (l:t list) = t

module Infix = 
struct
  let (&) = _and 
  let (+) = _or
  let (!) = _not
  let (||) =_given
  let (^) = _in 
end

