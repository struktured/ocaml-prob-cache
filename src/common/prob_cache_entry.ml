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
      module Value : VALUE
      module Or_error : OR_ERROR
      type flow = [`Stop | `Continue] [@@deriving show]
      type t = flow * Value.t Or_error.t [@@deriving show]
      val stop : Value.t Or_error.t -> t
      val continue : Value.t Or_error.t -> t
      module Error :
      sig
       val stop : Or_error.Error.t -> t
       val continue : Or_error.Error.t -> t
      end
      module Ok :
      sig
       val stop : Value.t -> t
       val continue : Value.t -> t
      end
      val value : t -> Value.t Or_error.t
      val flow : t -> flow
    end
    module Make(Value:VALUE)(Or_error:OR_ERROR) : S with module Value = Value =
    struct
      module Value = Value
      module Or_error = Or_error
      type flow = [`Stop | `Continue] [@@deriving show]
      type t = flow * Value.t Or_error.t
      let stop t = `Stop, t
      let continue t = `Continue,t
      let flow (f, _) = f
      let value (_, v) = v
      module Ok =
      struct
       let stop v = stop @@ Or_error.return v
       let continue v = continue @@ Or_error.return v
      end

    end
  end

  module type S =
  sig
    module Entry : ENTRY
    module Return : Return.S
    module Value = Return.Value
    type t = accum:Value.t -> Entry.t -> Return.t
  end

  (** Creates stateless predicate functions given an entry module *)
  module Make(Entry:ENTRY) (Value:VALUE) : S with
    module Entry = Entry and
    module Return.Value = Value (* and
    module Or_error.Or_error = Or_error *) =
  struct
    module Entry = Entry
    module Value = Value
    module type RETURN = Return.S with module Value = Value
    module Return = struct include (Return.Make(Value) : RETURN) end
    type t = accum:Value.t -> Entry.t -> Return.t
end
end
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


