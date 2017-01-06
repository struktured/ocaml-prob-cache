(** A abstract model, defining the type of events and data structures
to maintain their probabilities and expectations.
*)
open Or_errors.Std
open Or_errors_containers.Std
open Prob_cache.Std

(** Floating point convenience module *)
module Float = Model_primitives.Float

(** Represents a single event- must be comparable and showable *)
module type EVENT =
  sig
    type t [@@deriving show, ord]
    include Events.EVENT with type t := t
  end

(** Represents an abstract collection of events *)
module type EVENTS =
sig
  type t [@@deriving show, ord]
  module Event : EVENT
  include Events.EVENTS with module Event := Event and type t := t
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

module Showable =
struct
  module type S = sig type t [@@deriving show] end
end

module Base_error(T:Showable.S) : ERROR with type t = T.t = struct
  include T
  let to_string_mach t = show t
  let to_string_hum t = show t
end

module Create_error = Base_error(struct type t = [`Create_error of string] [@@deriving show] end)
module Data_error = Base_error(struct type t = [`Data_error of string] [@@deriving show] end)
module Find_error = Base_error(struct type t = [`Find_error of string] [@@deriving show] end)
module Observe_error = Base_error(struct type t = [`Observe_error of string] [@@deriving show] end)

  module Error =
      struct
        type t =
          [ `Create_error of Create_error.t
          | `Observe_error of Observe_error.t
          | `Data_error of Data_error.t
          | `Find_error of Find_error.t] [@@deriving show]

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
  include Or_errors_containers.Std.Or_error.Make(Error)
end

module type S_KERNEL =
  sig
    module Events : EVENTS
    include Model_kernel.S with
      module Or_error = Or_error and
      module Events := Events
  end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
  sig
    module Events : EVENTS
    module Event = Events.Event
    module Or_error : module type of Or_error
    include Model_decorator.S with
      module Events := Events and
      (*module Event := Event and*)
      module Or_error := Or_error
  end
