open Or_errors.Std
open Events_common

module type DATA_FUN = Data_fun.S
module type OBSERVE_DATA_FUN = Observe_data_fun.S
module type S =
sig
  include Model_kernel.S

  type prob = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type exp = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type var = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type sum = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type max = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type min = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type last = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type observe = ?cnt: int -> ?exp:float -> Events.t -> t -> t Observe_data_or_error.t

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

end

module Make 
  (*(Data_fun:DATA_FUN) 
  (Observe_data_fun : OBSERVE_DATA_FUN with
  type t = Data_fun.t and
  module Or_error.Result = Data_fun.Or_error.Result and
  module Events = Data_fun.Events and
  module Data = Data_fun.Data) *)

  (Model_kernel : Model_kernel.S)
   : S (* with
    module Observe_error_converterData_error = Data_error_converter.Error_out and
      module Observe_error = Observe_error_converter.Error_out *)
  = struct
  include Model_kernel
  type prob = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type exp = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type var = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type sum = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type max = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type min = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type last = ?cond:Events.t -> Events.t -> t -> float Data_or_error.t

  type observe = ?cnt: int -> ?exp:float -> Events.t -> t -> t Observe_data_or_error.t


  module Data_error = Data_or_error.Error
  module Observe_data_error = Observe_data_or_error.Error

  (*include (Data_fun : DATA_FUN with
  include (Observe_fun : OBSERVE_DATA_FUN with 
    type t := t and 
    module Events := Events and
    module Result := Result and
    module Data := Data)*)

  let prob ?(cond:Events.t option) (events:Events.t) (t:t) = failwith("NYI")

  let _data_map ?(cond=Events.empty) events t f =
    Data_or_error.map f (data (Events.join cond events) t)

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
     observe_data
      (Data.create ~cnt ~exp) events t
end

