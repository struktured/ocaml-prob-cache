open Or_errors.Std
open Events_common
module type S =
  sig

  type t
  module Or_error : OR_ERROR

  module Entry : Prob_cache_entry.S

  module Make : functor(Mapper:Mapper with module ) -> 
    
  sig
    val map : t -> mapper:Mapper.t -> Entry.t * Result.t Or_error.t
    (** Maps all observed events given an optional predicate filter function from the model. *)
  end
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
