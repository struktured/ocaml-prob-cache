open Or_errors.Std
open Prob_cache_events

module Data = Prob_cache_data
module type DATA = Data.S

module Entry = Prob_cache_entry
module type ENTRY = Entry.S

module Make(Entry:ENTRY) (*(Or_error:OR_ERROR) *) =
struct
  module Entry = Entry
  module Or_error = Or_error
  module Return =
  struct
    module type S =
    sig
      module Flow :
      sig
        type t = [`Stop | `Continue] [@@deriving show]
      end
      type 'a t = Flow.t * 'a
      val stop : 'a -> 'a t
      val continue : 'a -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      (*module Error :
      sig
        val stop : Or_error.Error.t -> 'a t
        val continue : Or_error.Error.t -> 'a t
      end
      module Ok :
      sig
        val stop : 'a -> 'a t
        val continue : 'a -> 'a t
      end *)

      val value : 'a t  -> 'a
      val flow : 'a t -> Flow.t
    end
    module I =
    struct
      module Flow =
      struct
        type t = [`Stop | `Continue] [@@deriving show]
      end
      type 'a t = Flow.t * 'a
      let stop t = `Stop, t
      let continue t = `Continue,t
      let flow (f, _) = f
      let value (_, v) = v

      let map t ~f = let res = f @@ value t in
        flow t |> function `Stop -> stop res | `Continue -> continue res

      (* module Ok =
      struct
        let stop v = stop @@ Or_error.return v
        let continue v = continue @@ Or_error.return v
      end

      module Error =
      struct
        let stop e = stop @@ Or_error.fail e
        let continue e = continue @@ Or_error.fail e
      end *)
    end
    include (I : S)
  end
  type ('state, 'acc) t =
    state:'state -> 'acc -> Entry.t -> 'acc Return.t
end

module type S =
sig
  module Entry : ENTRY 
  (*module Or_error : OR_ERROR*)
  include module type of Make(Entry) with
    module Entry := Entry (*and
    module Or_error := Or_error *)
end
