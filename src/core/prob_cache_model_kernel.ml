open Or_errors.Std
open Prob_cache_events

module Data = Prob_cache_data
module Create_fun = Prob_cache_create_fun
module Observe_data_fun = Prob_cache_observe_data_fun
module Data_fun = Prob_cache_data_fun
module Fold_fun = Prob_cache_fold_fun

module type DATA = Data.S
module type CREATE_FUN = Create_fun.S
module type OBSERVE_DATA_FUN = Observe_data_fun.S
module type DATA_FUN = Data_fun.S
module type FOLD_FUN = Fold_fun.S

module type S =
sig

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** The module type representing statistical data bound to some events *)
  module Data : DATA

  (** An abstract events model cache *)
  type t

  (* An or_error monad, possibly deferred *)
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

  include FOLD_FUN with
    module Entry.Events = Events and
    module Or_error := Or_error and
    module Entry.Data = Data and
    type t := t
end

module Make
  (Events:EVENTS) (** TODO why is this Events arg required? *)
  (Create_fun:CREATE_FUN with module Events = Events)
  (Data_fun:DATA_FUN with
    type t = Create_fun.t and
    module Events = Events and
    module Or_error = Create_fun.Or_error and
    module Data = Create_fun.Data)
  (Observe_data_fun:OBSERVE_DATA_FUN with
    type t = Create_fun.t and
    module Events = Events and
    module Or_error = Create_fun.Or_error and
    module Data = Create_fun.Data)
  (Fold_fun:FOLD_FUN with
    type t = Create_fun.t and
    module Entry.Events = Events and
    module Or_error = Create_fun.Or_error and
    module Entry.Data = Create_fun.Data)
: S with
  type t = Create_fun.t and
  module Events = Events and
  module Or_error = Create_fun.Or_error and
  module Data = Create_fun.Data and
  module Entry.Events = Events and
  module Entry.Data = Create_fun.Data
(* and module Entry.Or_error = Create_fun.Or_error *) =
struct
  include Create_fun
  include (Data_fun : DATA_FUN with
    module Events := Events and
    module Or_error := Or_error and
    module Data := Data and
    type t := Data_fun.t)

  include (Observe_data_fun : OBSERVE_DATA_FUN with
    module Events := Events and
    module Or_error := Or_error and
    module Data := Data and
    type t := Observe_data_fun.t)

  include (Fold_fun : FOLD_FUN with
    module Entry.Events = Events and
    module Or_error := Or_error and
    module Entry.Data = Data and
    type t := Fold_fun.t)
end
