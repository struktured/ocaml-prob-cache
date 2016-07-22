open Or_errors.Std
open Events_common
module type DATA = Data.S
module type S =
  sig

  type t
  module Or_error : OR_ERROR

  (** Container for the descriptive statistics **)
  module Data : DATA

  module Entry : Prob_cache_entry.S
  (** Gets all observed events given a filter function from the model. *)

  module Folder : Prob_cache_entry.Folder.S with
    module Entry := Entry
  val ('init, 'accum) fold : ('init, 'accum) Folder.fold
  end

module Fold_error_converter
  (Error_in : ERROR)
  (Error_out : sig
    include ERROR
    val of_fold : Error_in.t -> t end) : ERROR_CONVERTER with
    module Error_in = Error_in and
    module Error_out = Error_out =
  struct
     module Error_in = Error_in
     module Error_out = Error_out
     let convert (e:Error_in.t) = Error_out.of_fold e
  end
