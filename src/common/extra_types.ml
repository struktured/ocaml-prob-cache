open Or_errors.Std
open Events_common

module type DATA_FUN = Data_fun.S
module type OBSERVE_DATA_FUN = Observe_data_fun.S
module type S =
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

module Make_extra_types(Data_fun : Data_fun.S)
  (Observe_data_fun : OBSERVE_DATA_FUN with
    module Observe_data_or_error.Result = Data_fun.Data_or_error.Result) : S with
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


module EXTRA(Extra_types:S) = struct

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

module EXTRA_TYPES_OF(Data_fun:Data_fun.S)
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

