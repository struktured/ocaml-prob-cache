open Or_errors.Std
open Events_common
module Entry = Prob_cache_entry
module Mapper = Entry.Mapper

module type S =
sig
  type t
  module Entry : Entry.S
  module Or_error : OR_ERROR

  module Create : sig
    module Or_error :
    sig
      module Result : sig type t [@@deriving show] end
      module Make : functor(Mapper: Mapper.S with module Entry = Entry and module Or_error = Or_error) ->
      sig
        val map : t -> f:Mapper.t -> Mapper.Result.t Or_error.t list
        (** Maps all observed events given an optional predicate filter function from the model. *)
      end
  end

  module Make : functor(Mapper: Mapper.S with module Entry = Entry) ->
  sig
    val map : t -> f:Mapper.t -> Mapper.Result.t list Or_error.t
    (** Maps all observed events given an optional predicate filter function from the model. *)
  end

  module Optional :
  sig
    module Make : functor(Mapper: Mapper.Optional.S with module Entry = Entry) ->
    sig
      val map : t -> f:Mapper.t -> Mapper.Result.t list Or_error.t
      (** Maps all observed events given an optional predicate filter function from the model. *)
    end
  end
  end
  end
module Map_error_converter
  (Error_in : ERROR)
  (Error_out :
   sig include ERROR val of_map : Error_in.t -> t end) :
  ERROR_CONVERTER with
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_map e
  end
