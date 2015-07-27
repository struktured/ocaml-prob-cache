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

module Base_error(T:sig type t [@@deriving show] end) : Model_common.ERROR with type t = T.t = struct
  include T
  let to_string_mach t = show t
  let to_string_hum t = show t
end

module Create_error = Base_error(struct type t = [`Create_error of string] [@@deriving show] end)
module Data_error = Base_error(struct type t = [`Data_error of string] [@@deriving show] end)
module Find_error = Base_error(struct type t = [`Find_error of string] [@@deriving show] end)
module Observe_error = Base_error(struct type t = [`Observe_error of string] [@@deriving show] end)

module Or_errors = 
struct
  module Error =
      struct
        type t =
          [ `Create_error of Create_error.t
          | `Observe_error of Observe_error.t
          | `Data_error of Data_error.t
          | `Find_error of Find_error.t]

        let of_create e = `Create_error e
        let of_observe e = `Observe_error e
        let of_data e = `Data_error e
        let of_find e = `Find_error e

        let to_string_hum = function
          | `Create_error e -> Create_error.to_string_hum e
          | `Observe_error e -> Observe_error.to_string_hum e
          | `Data_error e -> Data_error.to_string_hum e
          | `Find_error e -> Find_error.to_string_hum e

        let to_string_mach = function
          | `Create_error e -> Create_error.to_string_mach e
          | `Observe_error e -> Observe_error.to_string_mach e
          | `Data_error e -> Data_error.to_string_mach e
          | `Find_error e -> Find_error.to_string_mach e

      end
      module Or_error =
      struct
        type 'a t = ('a, Error.t) Result.t
        let return x : 'a t = Result.return x
        let all x : 'a list t = Result.all x
        let bind f x : 'b t = Result.bind f x
        let map f x : 'b t = Result.map f x
        let both (x:'a t) (y: 'b t) : ('a * 'b) t = Result.both x y
        module Infix = 
          struct
            let (>>|) = Result.Monad_infix.(>>|)
            let (>|=) = Result.Monad_infix.(>|=)
            let (>>=) = Result.Monad_infix.(>>=)
          end
        let of_result (r: ('a, Error.t) Result.t) : 'a t = r
        end
end


(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig
  module Events : EVENTS
  include Model_common.S with 
    module Result = Result and
    module Events := Events 
end
