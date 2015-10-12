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

