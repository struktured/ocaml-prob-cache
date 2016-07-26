open Or_errors.Std
open Events_common
module type DATA = Data.S


module Fold =
struct
  module type ENTRY = Prob_cache_entry.S
  module Make(Entry:ENTRY)(Or_error:OR_ERROR) =
  struct
  module Return =
  struct
    module type S =
    sig
      module Flow :
      sig
        type t = [`Stop | `Continue] [@@deriving show]
      end
      type 'a t = Flow.t * 'a Or_error.t
      val stop : 'a Or_error.t -> 'a t
      val continue : 'a Or_error.t -> 'a t
      module Error :
      sig
       val stop : Or_error.Error.t -> 'a t
       val continue : Or_error.Error.t -> 'a t
      end
      module Ok :
      sig
       val stop : 'a -> 'a t
       val continue : 'a -> 'a t
      end
      val value : 'a t  -> 'a Or_error.t
      val flow : 'a t -> Flow.t
    end
    module I =
    struct
      module Flow =
      struct
        type t = [`Stop | `Continue] [@@deriving show]
      end
      type 'a t = Flow.t * 'a Or_error.t
      let stop t = `Stop, t
      let continue t = `Continue,t
      let flow (f, _) = f
      let value (_, v) = v
      module Ok =
      struct
       let stop v = stop @@ Or_error.return v
       let continue v = continue @@ Or_error.return v
      end

      module Error =
      struct
       let stop v = stop @@ Or_error.fail v
       let continue v = continue @@ Or_error.fail v
      end
    end 
    include (I: S)
    end
    type ('init, 'accum) t =
      init:'init -> accum:'accum -> Entry.t -> 'accum Return.t
    end
  module type S =
    sig
      module Entry : ENTRY
      module Or_error : OR_ERROR
      include module type of Make(Entry)(Or_error)
    end
end

module type S =
  sig

  type t
  module Or_error : OR_ERROR

  module Entry : Prob_cache_entry.S
  (** Gets all observed events given a filter function from the model. *)
  module Fold : Fold.S
  val fold : t -> f:('init, 'accum) Fold.t -> init:'init -> 'accum Or_error.t
  (*val ('init, 'accum) fold : ('init, 'accum) Folder.fold *)
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
