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
  module Create_or_error : OR_ERROR

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

  type create =
    ?update_rule:update_rule -> ?prior_count:prior_count ->
      ?prior_exp:prior_exp -> name:string -> t Create_or_error.t

  val create : create
  (** Creates a new model cache labeled by the given string. By default, expectations are updated
     using a mean value estimator and all priors are value 0. *)

  val name : t -> string
  (** Gets the name of the cache *)


end

module type OBSERVE_DATA_FUN =
sig

  type t

  module Observe_data_or_error : OR_ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  type observe_data = Data.t -> Events.t -> t -> t Observe_data_or_error.t

  val observe_data : observe_data
  (** Observe events from a [data] instance of descriptive statistics. This
    can be used to batch updates, or to load independently generated datasets
    together in a meaningful way. The returned model reflects the observation
    updates while the original instance is not guaranteed to be current. *)
end

module type DATA_FUN =
sig

  type t
  module Data_or_error : OR_ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  type data = Events.t -> t -> Data.t Data_or_error.t

  val data : data
  (** Gets the descriptive statistics data for the given events.
      Returns data with count of zero otherwise and other values set to nan. *)

end

module type FIND_FUN =
sig

  type t
  module Find_or_error : OR_ERROR

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  type find = (Events.t -> bool) -> t -> Events.t Find_or_error.t
  (** Gets all observed events given a filter function from the model. *)

  val find : find
  (** Gets all observed events given a filter function from the model. *)

end

module type EXTRA_TYPES =
sig
  type t
  module Events : EVENTS
  module Result : RESULT
  module Data_or_error : OR_ERROR with module Result = Result
  module Observe_data_or_error : OR_ERROR with module Result = Result

  type prob = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type exp = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type var = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type sum = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type max = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type min = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type last = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type observe = ?cnt: int -> ?exp:float -> Events.t -> t -> t Observe_data_or_error.t
end

module Make_extra_types(Data_fun : DATA_FUN)
  (Observe_data_fun : OBSERVE_DATA_FUN with
    module Observe_data_or_error.Result = Data_fun.Data_or_error.Result) : EXTRA_TYPES with
  module Result = Data_fun.Data_or_error.Result and
  module Result = Observe_data_fun.Observe_data_or_error.Result and
  module Events = Data_fun.Events and
  module Data_or_error = Data_fun.Data_or_error and
  module Observe_data_or_error = Observe_data_fun.Observe_data_or_error and
  type t = Data_fun.t =
struct
  include Data_fun
  module Result = Data_or_error.Result
  module Observe_data_or_error = Observe_data_fun.Observe_data_or_error
  type prob = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type exp = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type var = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type sum = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type max = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type min = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type last = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type observe = ?cnt:int -> ?exp:float -> Events.t -> t -> t Observe_data_or_error.t
end


module type ERROR_CONVERTER =
sig
  module Error_in : ERROR
  module Error_out : ERROR
  val convert : Error_in.t -> Error_out.t
end

module EXTRA(Extra_types:EXTRA_TYPES) = struct

  module type S = sig
  open Extra_types
  val prob : prob
  (** Probability of events given [cond], possibly the empty events *)

  val exp : exp
  (** Expectation of events given [cond], possibly the empty events *)

  val var : var
  (** Statistical variance of events given [cond], possibly the empty events *)

  val sum : sum
  (** Aggregated sum of events given [cond], possibly the empty events *)

  val max : max
  (** Observed maximum of events given [cond], possibly the empty events *)

  val min : min
  (** Observed minimum of events given [cond], possibly the empty events *)

  val last : last
  (** Observed last value of events given [cond], possibly the empty events *)

  val observe : observe
  (** Observe new events *)

  end
end

module Identity_error_converter(Error_in:ERROR) : ERROR_CONVERTER with
  module Error_in = Error_in and module Error_out = Error_in =
  struct
     module Error_in = Error_in
     module Error_out = Error_in
     let convert (e:Error_in.t) : Error_out.t = e
  end

module EXTRA_TYPES_OF(Data_fun:DATA_FUN)
 (Observe_data_fun : OBSERVE_DATA_FUN with
  type t = Data_fun.t and
  module Observe_data_or_error.Result = Data_fun.Data_or_error.Result and
  module Events = Data_fun.Events and
  module Data = Data_fun.Data) = EXTRA(Make_extra_types(Data_fun)(Observe_data_fun))

  (** ..._converter.Error_out is the actual error of interest. The identity
 * converter is used to create the Error_in versions *)
module Make_extra (Data_fun:DATA_FUN) (Observe_data_fun : OBSERVE_DATA_FUN with
  type t = Data_fun.t and
  module Observe_data_or_error.Result = Data_fun.Data_or_error.Result and
  module Events = Data_fun.Events and
  module Data = Data_fun.Data)
  (Data_error_converter : ERROR_CONVERTER with
    module Error_in = Data_fun.Data_or_error.Error)
  (Observe_error_converter : ERROR_CONVERTER with
    module Error_in = Observe_data_fun.Observe_data_or_error.Error) : EXTRA_TYPES_OF(Data_fun)(Observe_data_fun).S (* with
    module Observe_error_converterData_error = Data_error_converter.Error_out and
      module Observe_error = Observe_error_converter.Error_out *)
  = struct

  module Data_error = Data_error_converter.Error_out
  module Observe_error = Observe_error_converter.Error_out

  module Extra_types = Make_extra_types(Data_fun)(Observe_data_fun)
  (*include (Data_fun : DATA_FUN with
  include (Observe_fun : OBSERVE_DATA_FUN with 
    type t := t and 
    module Events := Events and
    module Result := Result and
    module Data := Data)*)

  module Data = Data_fun.Data
  open Extra_types
  let prob ?(cond:Events.t option) (events:Events.t) (t:t) = failwith("NYI")

  let _data_map ?(cond=Events.empty) events t f =
     let open Data_or_error in
     match Data_or_error.map f (Data_fun.data (Events.join cond events) t) with
     | (Data_or_error.Result.Ok _ as o) -> o
     | Data_or_error.Result.Error e -> Data_or_error.of_result
      (Data_or_error.Result.Error (Data_error_converter.convert e))

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
     let result = Observe_data_fun.observe_data
      (Observe_data_fun.Data.create ~cnt ~exp) events t in
     match result with
     | (Ok _ as o) -> o
     | Error e -> (Error (Observe_error_converter.convert e))
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S_BASE =
sig

  module Result : RESULT

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** The module type representing one event *)
  module Event = Events.Event

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
end

module type OR_ERRORS =
sig
  module Error : ERROR
  include S_BASE with module Create_error := Error and module Observe_error := Error and module Find_error := Error
  module Or_error : OR_ERROR with module Error = Error and module Result = Result
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig 
  include S_BASE
  module Or_errors : OR_ERRORS with
   module Result = Result and
   module Events = Events and
   module Data = Data
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module S_BASE_EXTRA(Extra_types:EXTRA_TYPES) =
struct module type S =
sig 
  include S_BASE
  include EXTRA(Extra_types).S with module Data_error := Data_error and module Observe_error := Observe_error
(*  module Or_errors :
    sig
      include OR_ERRORS with 
        module Result = Result and 
        module Events = Events and
        module Data = Data 
      include EXTRA(Extra_poly).S with 
        module Data_error := Data_error and module Observe_error := Observe_error
    end *)
end
end

module OR_ERRORS_EXTRA(Extra_types:EXTRA_TYPES) =
struct
  module type S =
    sig
      module Error : ERROR
      include S_BASE_EXTRA(Extra_types).S with
        module Create_error := Error and
        module Observe_error := Error and
        module Find_error := Error
      module Or_error : OR_ERROR with module Error = Error and module Result = Result
    end
end

module S_EXTRA(Extra_types:EXTRA_TYPES) =
struct module type S =
sig
  include S_BASE
  include EXTRA(Extra_types).S with module Data_error := Data_error and module Observe_error := Observe_error
  module Or_errors :
    sig
      include OR_ERRORS_EXTRA(Extra_types).S with
        module Result = Result and
        module Events = Events and
        module Data = Data
      include EXTRA(Extra_types).S with
        module Data_error := Data_error and module Observe_error := Observe_error
    end
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

module Or_error_of_result(Result:RESULT)(Error:ERROR) :
    OR_ERROR with
      module Result = Result and
      module Error = Error =
  struct
      type 'a t = ('a, Error.t) Result.t
      module Error = Error
      module Result = Result
      let map = Result.map
      let bind = Result.bind
      let return = Result.return
      let all = Result.all
      let both = Result.both
      module Infix = Result.Monad_infix
      let of_result t = t
  end

module Or_error_of(Or_error:OR_ERROR)(Error:ERROR) = Or_error_of_result(Or_error.Result)(Error)


module Make
  (Or_error : OR_ERROR)
  (Events : EVENTS)
  (Data : DATA)
  (Create_fun : CREATE_FUN with
      module Create_or_error.Result = Or_error.Result and
      module Events = Events and
      module Data = Data)
  (Observe_fun : OBSERVE_DATA_FUN with
      module Observe_data_or_error.Result = Or_error.Result and
      module Events = Events and
      module Data = Data and
      type t = Create_fun.t)
  (Data_fun : DATA_FUN with
      module Data_or_error.Result = Or_error.Result and
      module Events = Events and
      module Data = Data and
      type t = Create_fun.t)
  (Find_fun : FIND_FUN with
      module Find_or_error.Result = Or_error.Result and
      module Events = Events and
      module Data = Data and
      type t = Create_fun.t)
  (Create_error_converter : ERROR_CONVERTER with
    module Error_in = Create_fun.Create_or_error.Error and module Error_out = Or_error.Error)
  (Data_error_converter : ERROR_CONVERTER with
    module Error_in = Data_fun.Data_or_error.Error and module Error_out = Or_error.Error)
  (Observe_error_converter : ERROR_CONVERTER with
    module Error_in = Observe_fun.Observe_data_or_error.Error and module Error_out = Or_error.Error)
  (Find_error_converter : ERROR_CONVERTER with
    module Error_in = Find_fun.Find_or_error.Error and module Error_out = Or_error.Error)
 (* : S_EXTRA(Make_extra_poly(Data_fun)(Observe_fun)).S with
  module Result = Result and
  module Events = Events and
  module Data = Data and
  module Or_errors.Error = Or_error.Error and
  module Or_errors.Or_error = Or_error *) =
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
    module Observe_error = Observe_fun.Observe_error and
    type t := t)
  include (Data_fun : DATA_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    module Data_error = Data_fun.Data_error and
    type t := t)
  include (Find_fun : FIND_FUN with
    module Events := Events and
    module Result := Result and
    module Data := Data and
    module Find_error = Find_fun.Find_error and
    type t := t)
  module Or_errors_extra = OR_ERRORS_EXTRA(Make_extra_poly(Data_fun)(Observe_fun))
  module Or_errors : Or_errors_extra.S with
    module Result = Result and 
    module Error = Error and
    module Or_error = Or_error and
    module Events = Events and
    module Data = Data =
  struct
    type t = Data_fun.t
    (** Defines a prior function in terms of counts with the observed events as input. *)
    type prior_count = Create_fun.prior_count

    (** Define a prior function in terms of real values with the observed events as input. *)
    type prior_exp = Create_fun.prior_exp

    (** Defines the update rule for expectations *)
    type update_rule = Create_fun.update_rule 

    type 'a observe_data = 'a Observe_fun.observe_data
    type 'a data = 'a Data_fun.data
    type 'a find = 'a Find_fun.find
    type 'a create = 'a Create_fun.create
    module Result = Result
    module Events = Events
    module Event = Event
    module Error = Or_error.Error
    module Or_error = Or_error
    module Data = Data
    let create ?update_rule ?prior_count ?prior_exp ~name =
      create ?update_rule ?prior_count ?prior_exp ~name |> function
      (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Create_error_converter.convert e) |>
      Or_error.of_result

    let observe_data data events t = observe_data data events t |> function
      (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Observe_error_converter.convert e) |>
      Or_error.of_result

    let data events t = data events t |> function
      (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Data_error_converter.convert e) |>
      Or_error.of_result

    let find f t = find f t |> function
      (Result.Ok _) as o -> o | Result.Error e -> Result.Error (Find_error_converter.convert e) |>
      Or_error.of_result

    let update_rule = update_rule
    let name = name

    module Extra_or_error :
      EXTRA_TYPES_OF(Data_fun)(Observe_fun).S
      with module Data_error = Data_error_converter.Error_out and module Observe_error = Observe_error_converter.Error_out
      = Make_extra(Data_fun)(Observe_fun)(Data_error_converter)(Observe_error_converter)
    include Extra_or_error
  end

  module Extra : EXTRA_TYPES_OF(Data_fun)(Observe_fun).S with
    module Data_error := Data_fun.Data_error and
    module Observe_error := Observe_fun.Observe_error = Make_extra
    (Data_fun)(Observe_fun)
    (Identity_error_converter(Data_fun.Data_error))
    (Identity_error_converter(Observe_fun.Observe_error))

  include Extra

end

