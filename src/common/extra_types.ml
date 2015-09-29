open Or_errors.Std
open Events_common
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


