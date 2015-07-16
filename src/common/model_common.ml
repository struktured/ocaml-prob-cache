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

module type OR_ERROR =
 sig
   module Error : ERROR
   module Result : RESULT
   include MONAD with type 'a t = ('a, Error.t) Result.t
   val of_result : ('a, Error.t) Result.t -> 'a t
end


module type CREATE_FUN =
sig

  type t
  module Result : RESULT
  module Create_error : ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

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
  module Data : DATA

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

  type 'err observe = ?cnt:int -> ?exp:float -> Events.t -> t -> (t, 'err) Result.t
end


module type ERROR_CONVERTER =
sig
  module Error_in : ERROR
  module Error_out : ERROR
  val convert : Error_in.t -> Error_out.t
end

module type EXTRA_FUNCTOR = functor
  (Data_error_converter : ERROR_CONVERTER)
  (Observe_error_converter : ERROR_CONVERTER)
-> 
sig
  module Extra_poly : EXTRA_POLY
  module Data_error : module type of Data_error_converter.Error_out 
  module Observe_error : module type of Observe_error_converter.Error_out
  open Extra_poly
  val prob : Data_error.t prob
  (** Probability of events given [cond], possibly the empty events *)

  val exp : Data_error.t exp
  (** Expectation of events given [cond], possibly the empty events *)

  val var : Data_error.t var
  (** Statistical variance of events given [cond], possibly the empty events *)

  val sum : Data_error.t sum
  (** Aggregated sum of events given [cond], possibly the empty events *)

  val max : Data_error.t max
  (** Observed maximum of events given [cond], possibly the empty events *)

  val min : Data_error.t min
  (** Observed minimum of events given [cond], possibly the empty events *)

  val last : Data_error.t last
  (** Observed last value of events given [cond], possibly the empty events *)
 
  val observe : Observe_error.t observe 
  
end

module Identity_error_converter(Error_in:ERROR) : ERROR_CONVERTER with 
  module Error_in = Error_in and module Error_out = Error_in =
  struct
     module Error_in = Error_in
     module Error_out = Error_in
     let convert (e:Error_in.t) : Error_out.t = e
  end

module Make_extra (Data_fun:DATA_FUN) (Observe_fun : OBSERVE_DATA_FUN with 
  type t = Data_fun.t and 
  module Result = Data_fun.Result and 
  module Events = Data_fun.Events and
  module Data = Data_fun.Data) (*: 
EXTRA_FUNCTOR*) = functor
  (Data_error_converter : ERROR_CONVERTER with 
    module Error_in = Data_fun.Data_error and type Error_in.t = Data_fun.Data_error.t)
  (Observe_error_converter: ERROR_CONVERTER with 
    module Error_in = Observe_fun.Observe_error and type Error_in.t = Observe_fun.Observe_error.t)
  -> struct
  module Extra_poly = Make_extra_poly(Data_fun)(Observe_fun)
  include Data_fun
  include (Observe_fun : OBSERVE_DATA_FUN with 
    type t := t and 
    module Events := Events and
    module Result := Result and
    module Data := Data)

  open Extra_poly
  let prob ?cond events t = 
     failwith("NYI")

    
   let _data_map ?(cond=Events.empty) events t f : (float, Data_error_converter.Error_out.t) Result.t = 
     let open Result in
     match Result.map f (data (Events.join cond events) t) with
     | (Ok _ as o) -> o
     | Error e -> Error (Data_error_converter.convert e)

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

   let observe ?(cnt=1) ?(exp=1.0) events t =
     let open Result in
     let result = Observe_fun.observe_data 
      (Observe_fun.Data.create ~cnt ~exp) events t in
     match result with
     | (Ok _ as o) -> o
     | Error e -> (Error (Observe_error_converter.convert e))
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
        include ERROR
        val of_create : Create_error.t -> t
        val of_observe : Observe_error.t -> t
        val of_data : Data_error.t -> t
        val of_find : Find_error.t -> t
      end

      module Or_error : OR_ERROR with
        module Error = Error and module Result = Result

      val update_rule : t -> update_rule
      val create : Error.t create
      val observe_data : Error.t observe_data
      val data : Error.t data
      val find : Error.t find

      val name : t -> string
  end
end

module Create_error_converter 
  (Error_in : ERROR)
  (Error_out : sig include ERROR val of_create : Error_in.t -> t end) : ERROR_CONVERTER with 
    module Error_in = Error_in and 
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_create e
  end

module Observe_error_converter
  (Error_in : ERROR)
  (Error_out : sig include ERROR val of_observe : Error_in.t -> t end) : ERROR_CONVERTER with 
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_observe e
  end

module Data_error_converter
  (Error_in : ERROR)
  (Error_out : sig include ERROR val of_data : Error_in.t -> t end) : ERROR_CONVERTER with 
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_data e
  end

module Find_error_converter
  (Error_in : ERROR)
  (Error_out : sig include ERROR val of_find : Error_in.t -> t end) : ERROR_CONVERTER with 
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_find e
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
  (Or_error : OR_ERROR)

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
      module Error = Or_error.Error
      module Or_error = Or_error

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

