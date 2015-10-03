(** An abstract model, defining the type of events and data structures
    to maintain their probabilities and expectations. *)

(** Common Interfaces and Functors for creating models *)

(** Floating point convenience module *)
module Float = CCFloat
module type DATA = Data.S
open Events_common
open Or_errors.Std

module type CREATE_FUN = Create_fun.S

module type OBSERVE_DATA_FUN = Observe_data_fun.S
module type DATA_FUN = Data_fun.S

module type FIND_FUN = Find_fun.S

module type MODEL_DECORATOR = Model_decorator.S


module type S_KERNEL =
sig

  module Result : RESULT

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** The module type representing one event *)
  module Event = Events.Event

  module Data : DATA
    (** An abstract events model cache *)
  type t

  module Create_or_error : OR_ERROR

  module Data_or_error : OR_ERROR

  module Observe_data_or_error : OR_ERROR

  module Find_or_error : OR_ERROR


  include CREATE_FUN with
    module Events := Events and
    module Or_error := Create_or_error and
    module Data := Data and
    type t := t

  include DATA_FUN with
    module Events := Events and
    module Or_error := Data_or_error and
    module Data := Data and
    type t := t

  include OBSERVE_DATA_FUN with
    module Events := Events and
    module Or_error := Observe_data_or_error and
    module Data := Data and
    type t := t

  include FIND_FUN with
    module Events := Events and
    module Or_error := Find_or_error and
    module Data := Data and
    type t := t
end

module type OR_ERRORS =
sig
  module Error : ERROR
  include S_KERNEL with module Create_error := Error and module Observe_error := Error and module Find_error := Error
  module Or_error : OR_ERROR with module Error = Error and module Result = Result
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module type S =
sig 
  include S_KERNEL
  module Or_errors : OR_ERRORS with
   module Result = Result and
   module Events = Events and
   module Data = Data
end

(** A module type provided polymorphic probability model caches. Uses in memory models backed by the containers api *)
module S_KERNEL_EXTRA(Model_decorator:Model_decorator.S) =
struct module type S =
sig 
  include S_KERNEL
  include EXTRA(Model_decorator).S with module Data_error := Data_error and module Observe_error := Observe_error
(*  module Or_errors :
    sig
      include OR_ERRORS with 
        module Result = Result and 
        module Events = Events and
        module Data = Data 
      include EXTRA(Model_decorator_poly).S with 
        module Data_error := Data_error and module Observe_error := Observe_error
    end *)
end
end

module OR_ERRORS_EXTRA(Model_decorator:MODEL_DECORATOR) =
struct
  module type S =
    sig
      module Error : ERROR
      include S_KERNEL_EXTRA(Model_decorator).S with
        module Create_error := Error and
        module Observe_error := Error and
        module Find_error := Error
      module Or_error : OR_ERROR with module Error = Error and module Result = Result
    end
end

module S_EXTRA(Model_decorator:MODEL_DECORATOR) =
struct module type S =
sig
  include S_KERNEL
  include EXTRA(Model_decorator).S with module Data_error := Data_error and module Observe_error := Observe_error
  module Or_errors :
    sig
      include OR_ERRORS_EXTRA(Model_decorator).S with
        module Result = Result and
        module Events = Events and
        module Data = Data
      include EXTRA(Model_decorator).S with
        module Data_error := Data_error and module Observe_error := Observe_error
    end
end
end

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

    module Model_decorator_or_error :
      MODEL_DECORATOR_OF(Data_fun)(Observe_fun).S
      with module Data_error = Data_error_converter.Error_out and module Observe_error = Observe_error_converter.Error_out
      = Make_extra(Data_fun)(Observe_fun)(Data_error_converter)(Observe_error_converter)
    include Model_decorator_or_error
  end

  module Model_decorator : MODEL_DECORATOR_OF(Data_fun)(Observe_fun).S with
    module Data_error := Data_fun.Data_error and
    module Observe_error := Observe_fun.Observe_error = Make_extra
    (Data_fun)(Observe_fun)
    (Identity_error_converter(Data_fun.Data_error))
    (Identity_error_converter(Observe_fun.Observe_error))

  include Model_decorator

end

