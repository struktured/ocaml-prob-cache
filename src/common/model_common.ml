(** An abstract model, defining the type of events and data structures
    to maintain their probabilities and expectations. *)

(** Common Interfaces and Functors for creating models *)

(** Floating point convenience module *)
module Float = CCFloat
module type DATA = Data.S
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
module type OBSERVE_DATA_FUN =
sig

  type t
  module Result : RESULT
  module Observe_error : ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  type 'err observe_data = Data.t -> Events.t -> t -> (t, 'err) Result.t

  val observe_data : Observe_error.t observe_data
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
  module Data : DATA

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

module type EXTRA_POLY = 
sig
  type t
  module Events : EVENTS
  module Result : RESULT
  type 'err prob = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t 

  type 'err exp = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err var = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err sum = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err max = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err min = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err last = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err observe = ?cnt: int -> ?exp:float -> Events.t -> t -> (t, 'err) Result.t
end

module Make_extra_poly(Data_fun : DATA_FUN) (Observe_fun : OBSERVE_DATA_FUN) : EXTRA_POLY with 
  module Events = Data_fun.Events and 
  module Result = Data_fun.Result and
  type t = Data_fun.t = 
struct
  include Data_fun
  type 'err prob = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t 

  type 'err exp = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err var = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err sum = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err max = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err min = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err last = ?cond:Events.t -> Events.t -> t -> (float, 'err) Result.t

  type 'err observe = ?cnt: int -> ?exp:float -> Events.t -> t -> (t, 'err) Result.t
end

module type EXTRA_FUNCTOR = functor(Error : sig type t end) -> 
sig
  module Error : module type of Error
  module Extra_poly : EXTRA_POLY

  open Extra_poly
  val prob : Error.t prob
  (** Probability of events given [cond], possibly the empty events *)

  val exp : Error.t exp
  (** Expectation of events given [cond], possibly the empty events *)

  val var : Error.t var
  (** Statistical variance of events given [cond], possibly the empty events *)

  val sum : Error.t sum
  (** Aggregated sum of events given [cond], possibly the empty events *)

  val max : Error.t max
  (** Observed maximum of events given [cond], possibly the empty events *)

  val min : Error.t min
  (** Observed minimum of events given [cond], possibly the empty events *)

  val last : Error.t last
  (** Observed last value of events given [cond], possibly the empty events *)
 
  val observe : Error.t observe 

end

module Make_extra (Data_fun:DATA_FUN) (Observe_fun : OBSERVE_DATA_FUN with 
  type t = Data_fun.t and 
  module Result = Data_fun.Result and 
  module Events = Data_fun.Events and
  module Data = Data_fun.Data) : 
  EXTRA_FUNCTOR = functor(Error : sig type t end) -> 
struct
  module Extra_poly = Make_extra_poly(Data_fun)(Observe_fun)
  include Data_fun
  include (Observe_fun : OBSERVE_DATA_FUN with 
    type t := t and 
    module Events := Events and
    module Result := Result and
    module Data := Data)

  module Error = Error
  open Extra_poly
  let prob ?cond events t  = 
     failwith("NYI")

   let _data_map ?(cond=Events.empty) events t f : (float, 'err) Result.t = 
     Result.map f (data (Events.join cond events) t)

   let exp ?cond events t = 
     _data_map ?cond events t Data.expect

   let var ?cond events t =
     _data_map ?cond events t Data.var

   let sum ?cond events t =
     _data_map ?cond events t Data.sum

   let max ?cond events t =
     _data_map ?cond events t Data.max

   let min ?cond events t =
     _data_map ?cond events t Data.min

   let last ?cond events t =
     _data_map ?cond events t Data.last

   let observe ?(cnt=1) ?(exp=1.0) events t : (t, 'err) Result.t = 
     Observe_fun.observe_data (Observe_fun.Data.create ~cnt ~exp) events t
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig

  module Result : RESULT
  (** The module type representing one event *)
  module Event : EVENT

  (** The module type representing a collection of events *)
  module Events : EVENTS with module Event = Event

  module Data : DATA
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

  include OBSERVE_DATA_FUN with
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
      val observe_data : Error.t observe_data
      val data : Error.t data
      val find : Error.t find


      val name : t -> string
  end
end

module Make
  (Result : RESULT)
  (Events : EVENTS)
  (Data : DATA)
  (Create_fun :
    CREATE_FUN with 
      module Result := Result and 
      module Events := Events and
      module Data := Data)
  (Observe_fun : OBSERVE_DATA_FUN with
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
  include (Observe_fun : OBSERVE_DATA_FUN with
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

  let observe_data data events t = observe_data data events t |> function
    (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Error.of_observe e)

  let data events t = data events t |> function
    (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Error.of_data e)

  let find f t = find f t |> function
    (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Error.of_find e)

  let update_rule = update_rule
  let name = name
  end

end

