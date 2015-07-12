(** A abstract model, defining the type of events and data structures
to maintain their probabilities and expectations.
*)
open Prob_cache_common

(** Floating point convenience module *)
module Float = Model_common.Float

(** Represents a single event- must be comparable and showable *)
module type EVENT =
  sig  
    type t [@@deriving show, ord]
    include Events_common.EVENT with type t := t
  end

(** Represents an abstract collection of events *)
module type EVENTS =
sig
  type t [@@deriving show, ord]
  module Event : EVENT
  include Events_common.EVENTS with module Event := Event and type t := t 
end

module Data = struct
  module Ord_t =
    struct
      (** Compute running statitics using recurrence equations. *)
      type t = Running.t = { size : int         (** Number of observations. *)
      ; last : float        (** Last observation. *)
      ; max : float        (** Maxiumum. *)
      ; min : float        (** Minimum. *)
      ; sum : float        (** Sum . *)
      ; sum_sq : float     (** Sum of squares. *)
      ; mean : float      (** Mean. *)
      ; var : float (** _Unbiased_ variance *)
      } [@@deriving show, ord]
    end
  include Data.Make(Ord_t)
  let compare = Ord_t.compare
end

module Result : Model_common.RESULT = 
struct
  type ('ok, 'err) t = Ok of 'ok | Error of 'err
  let map f x = match x with (Error _) as e -> e | Ok y -> Ok (f y)
  let bind f x = match x with (Error _) as e -> e | Ok y -> (f y)
  let return x = Ok x
  let all elems = CCList.fold_while (fun l x -> match x with
      | Ok o -> 
          begin match l with (Error _ ) as e -> e | Ok l' -> Ok (o::l') end, 
        `Continue 
      | (Error _) as e -> e, `Stop) (Ok []) elems
  let both x y = match x,y with 
    | ((Error _) as e), _ -> e
    | _, ((Error _) as e') -> e'
    | Ok x, Ok y -> Ok (x, y)
  
  module Monad_infix = 
  struct
    let (>>=) x f = bind f x
    let (>>|) x f = map f x
    let (>|=) x f = map f x
  end
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig
  module Events : EVENTS
  module Event : EVENT
  include Model_common.S with 
    module Result = Result and
    module Events = Events and
    module Event = Event
end
