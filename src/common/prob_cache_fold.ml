open Or_errors.Std
open Events_common
module type DATA = Data.S

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
        let stop e = stop @@ Or_error.fail e
        let continue e = continue @@ Or_error.fail e
      end
    end
    include (I : S)
  end
  type ('state, 'accum) t =
    state:'state -> accum:'accum -> Entry.t -> 'accum Return.t
end

module type S =
sig
  module Entry : ENTRY
  module Or_error : OR_ERROR
  include module type of Make(Entry)(Or_error)
end
