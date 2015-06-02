(** Represents a single event- must be showable *)  
module type EVENT = sig type t [@@deriving show] end

(** Represents an abstract collection of events *)
module type EVENTS = 
sig
(*  module type EVENT = EVENT *)
  module Event : EVENT
  type t [@@deriving show] 
  val is_empty : t -> bool
  val join: t -> t -> t
  val empty : t
  val of_list : Event.t list -> t
  val to_list : t -> Event.t list
  val subsets : t -> t list
end
