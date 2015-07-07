(** An abstract model, defining the type of events and data structures
    to maintain their probabilities and expectations. *)

(** Common Interfaces and Functors for creating models *)

(** Floating point convenience module *)
module Float = CCFloat

open Events_common

(** Monadic api with infix operators *)
module type MONAD =
  sig
    type 'a t
    val map :  ('a -> 'b) -> 'a t -> 'b t
    val bind : ('a -> 'b t) -> 'a t -> 'b t
    val return : 'a -> 'a t
    val all : 'a t list -> 'a list t
    val both : 'a t -> 'b t -> ('a * 'b) t
    module Infix :
      sig
        val (>>|) : 'a t -> ('a -> 'b) -> 'b t
        val (>|=) : 'a t -> ('a -> 'b) -> 'b t
        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      end
  end

(** Result module, in spirit of Core.Std.Result *)
module type RESULT =
  sig
    type ('ok, 'err) t = Ok of 'ok | Error of 'err 
    val map : ('ok -> 'res) -> ('ok, 'err) t -> ('res, 'err) t
    val bind : ('ok -> ('res, 'err) t) -> ('ok, 'err) t -> ('res, 'err) t
    val return: 'ok -> ('ok, 'err) t
    val all : ('ok, 'err) t list -> ('ok list, 'err) t
    val both : ('a, 'err) t -> ('b, 'err) t -> ('a * 'b, 'err) t

    module Monad_infix :
      sig
        val (>>|) : ('ok, 'err) t -> ('ok -> 'res) -> ('res, 'err) t
        val (>|=) : ('ok, 'err) t -> ('ok -> 'res) ->  ('res, 'err) t
        val (>>=) : ('ok, 'err) t -> ('ok -> ('res, 'err) t) -> ('res, 'err) t
      end
  end

module type ERROR =
sig
  type t
  val to_string_hum : t -> string
  val to_string_mach : t -> string
end

module type CREATE_FUN =
sig

  type t
  module Result : RESULT
  module Create_error : ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : Data.S

  (* Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (* Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float

  (** Defines the update rule for expectations *)
  type update_rule = Events.t Data.update_rule
  val update_rule : t -> update_rule

  type 'err create =
    ?update_rule:update_rule -> ?prior_count:prior_count ->
      ?prior_exp:prior_exp -> name:string -> (t, 'err) Result.t

  val create : Create_error.t create
  (** Creates a new model cache labeled by the given string. By default, expectations are updated
     using a mean value estimator and all priors are value 0. *)

  val name : t -> string
  (** Gets the name of the cache *)


end

module type OBSERVE_FUN =
sig

  type t
  module Result : RESULT
  module Observe_error : ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : Data.S

  type 'err observe = Data.t -> Events.t -> t -> (t, 'err) Result.t

  val observe : Observe_error.t observe
  (** Observe events from a [data] instance of descriptive statistics. This
    can be used to batch updates, or to load independently generated datasets
    together in a meaningful way. The returned model reflects the observation
    updates while the original instance is not guaranteed to be current. *)
end

module type DATA_FUN =
sig

  type t
  module Result : RESULT
  module Data_error : ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : Data.S

  type 'err data = Events.t -> t -> (Data.t, 'err) Result.t

  val data : Data_error.t data
  (** Gets the descriptive statistics data for the given events.
      Returns data with count of zero otherwise and other values set to nan. *)

end

module type FIND_FUN =
sig

  type t
  module Result : RESULT
  module Find_error : ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : Data.S

  type 'err find = (Events.t -> bool) -> t -> (Events.t, 'err) Result.t
  (** Gets all observed events given a filter function from the model. *)

  val find : Find_error.t find
  (** Gets all observed events given a filter function from the model. *)

end


(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig

  module Result : RESULT

  (** The module type representing one event *)
  module Event : EVENT

  (** The module type representing a collection of events *)
  module Events : EVENTS with module Event = Event

  module Data : Data.S
    (** An abstract events model cache *)
  type t

  include CREATE_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    type t := t

  include DATA_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    type t := t

  include OBSERVE_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    type t := t

  include FIND_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    type t := t
    (** Simpler interface that unifies the error type *)
  module Or_errors :
  sig
      module Error :
      sig
        type t =
          [ `Create_error of Create_error.t
          | `Observe_error of Observe_error.t
          | `Data_error of Data_error.t
          | `Find_error of Find_error.t]
        include ERROR with type t:= t
        val of_create : Create_error.t -> t
        val of_observe : Observe_error.t -> t
        val of_data : Data_error.t -> t
        val of_find : Find_error.t -> t
      end

      module Or_error :
      sig
        include MONAD with type 'a t = ('a, Error.t) Result.t
        val of_result : ('a, Error.t) Result.t -> 'a t
      end

      val update_rule : t -> update_rule
      val create : Error.t create
      val observe : Error.t observe
      val data : Error.t data
      val find : Error.t find
      val name : t -> string
  end
end

module Make
  (Result : RESULT)
  (Events : EVENTS)
  (Data : Data.S)
  (Create_fun :
    CREATE_FUN with 
      module Result := Result and 
      module Events := Events and
      module Data := Data)
  (Observe_fun : OBSERVE_FUN with
      module Result := Result and
      module Events := Events and
      module Data := Data and
      type t := Create_fun.t)
  (Data_fun : DATA_FUN with
      module Result := Result and
      module Events := Events and
      module Data := Data and
      type t := Create_fun.t)
  (Find_fun : FIND_FUN with
      module Result := Result and
      module Events := Events and
      module Data := Data and
      type t := Create_fun.t)
 : S =
struct
  module Result = Result
  module Events = Events
  module Event = Events.Event
  module Data = Data
  include Create_fun
  include (Observe_fun : OBSERVE_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    type t := t)
  include (Data_fun : DATA_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    type t := t)
  include (Find_fun : FIND_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    type t := t)

  module Or_errors = struct
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

  let create ?update_rule ?prior_count ?prior_exp ~name =
    create ?update_rule ?prior_count ?prior_exp ~name |> function
    (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Error.of_create e) |>
    Or_error.of_result

  let observe data events t = observe data events t |> function
    (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Error.of_observe e)

  let data events t = data events t |> function
    (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Error.of_data e)

  let find f t = find f t |> function
    (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Error.of_find e)

  let update_rule = update_rule
  let name = name
  end

end

