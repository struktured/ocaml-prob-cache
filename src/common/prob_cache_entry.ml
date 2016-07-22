open Events_common
open Or_errors.Std
module type S =
sig
  type t
  module Events : EVENTS
  module Data : Data.S
  module Or_error : OR_ERROR
  val data : t -> Data.t Or_error.t
  val events : t -> Events.t Or_error.t
end

module Make (Events : EVENTS) (Data : Data.S) (Or_error : OR_ERROR) : S with
  module Events = Events and
  module Data = Data and
  module Or_error = Or_error =
struct
  type t = {events:Events.t;data:Data.t} [@@deriving make, show]
  module Events = Events
  module Data = Data
  module Or_error = Or_error
  let data t = Or_error.return t.data
  let events t = Or_error.return t.events
end
(*
module Predicate =
struct
  module type ENTRY = S
  module type S = sig module Entry : ENTRY type t val apply : t -> Entry.t -> bool end
  (** Creates stateless predicate functions given an entry module *)
  module Make(Entry:ENTRY)  : S with module Entry = Entry  =
  struct
    module Entry = Entry

    type t = {predicate:Entry.t -> bool}
    let create predicate = {predicate}
    let apply t = t.predicate
  end
end
*)

module Folder =
struct
  module type ENTRY = S
  module type VALUE = sig type t [@@deriving show] end

  module Return =
  struct
    module type S =
    sig
      module Or_error : OR_ERROR
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
    module Make(Or_error:OR_ERROR) : S =
    struct
      module Or_error = Or_error
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
  end

  module type S =
  sig
    module Entry : ENTRY
    module Return : Return.S
    type ('init, 'accum) fold =
      init:'init -> accum:'accum -> Entry.t -> 'accum Return.t
    val fold : ('init, 'accum) fold
  end

  module Make(Entry:ENTRY) (Or_error:OR_ERROR) : S with
    module Entry = Entry and module Return.Or_error = Or_error =
  struct
    module Return = Return.Make(Or_error)
    module Entry = Entry
    type ('init, 'accum) fold =
      init:'init -> accum:'accum -> Entry.t -> 'accum Return.t
    end

end
(*
module Predicate =
struct
  module type S =
  sig
    module Value : sig type t = bool [@@deriving show, ord] end
    include Folder.S with module Value := Value
  end
end

module List =
struct
  module type S =
  sig
    module Element : sig type t [@@deriving show] end
    module Value : sig type t = Element.t list [@@deriving show] end
    include Folder.S with module Value := Value
  end
end

module Array =
struct
  module type S =
  sig
    module Element : sig type t [@@deriving show] end
    module Value : sig type t = Element.t array [@@deriving show] end
    include Folder.S with module Value := Value
  end
end

module Vec =
struct
  module type S =
  sig
    module Value : sig type t = Lacaml_D.Vec.t [@@deriving show] end
    include Folder.S with module Value := Value
  end
end

module Float =
struct
  module type S =
  sig
    module Value : sig type t = float [@@deriving show] end
    include Folder.S with module Value := Value
  end
end
*)

