open Or_errors.Std
open Events_common
module type DATA = Data.S
module type CREATE_FUN = Create_fun.S

module type OBSERVE_DATA_FUN = Observe_data_fun.S
module type DATA_FUN = Data_fun.S
module type FIND_FUN = Find_fun.S

module type S =
sig

  module Result : RESULT

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** The module type representing one event *)
  module Event = Events.Event

  module Data : DATA

  (** An abstract events model cache *)
  type t

  module Or_error : OR_ERROR

  include CREATE_FUN with
    module Events := Events and
    module Or_error := Or_error and
    module Data := Data and
    type t := t

  include DATA_FUN with
    module Events := Events and
    module Or_error := Or_error and
    module Data := Data and
    type t := t

  include OBSERVE_DATA_FUN with
    module Events := Events and
    module Or_error := Or_error and
    module Data := Data and
    type t := t

  include FIND_FUN with
    module Events := Events and
    module Or_error := Or_error and
    module Data := Data and
    type t := t
end

module Make
  (Create_fun:CREATE_FUN)
  (Data_fun:DATA_FUN with
    module Events = Create_fun.Events and module Or_error = Create_fun.Or_error and module Data = Create_fun.Data)
  (Observe_data_fun:OBSERVE_DATA_FUN with
    module Events = Create_fun.Events and module Or_error = Create_fun.Or_error and module Data = Create_fun.Data)
  (Find_fun:FIND_FUN with
    module Events = Create_fun.Events and module Or_error = Create_fun.Or_error and module Data = Create_fun.Data) =
struct
  include Create_fun
  include (Data_fun : DATA_FUN with
    module Events := Events and module Or_error := Or_error and module Data := Data and type t := Data_fun.t)

  include (Observe_data_fun : OBSERVE_DATA_FUN with
    module Events := Events and module Or_error := Or_error and module Data := Data and type t := Observe_data_fun.t)

  include (Find_fun : FIND_FUN with
    module Events := Events and module Or_error := Or_error and module Data := Data and type t := Find_fun.t)
end

