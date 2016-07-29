open Prob_cache_events
module Data = Prob_cache_data
open Or_errors.Std

module type S_BASE =
sig
  type t
  module Events : EVENTS
  module Data : Data.S
  val data : t -> Data.t
  val events : t -> Events.t
end

module Predicate =
struct
  module type ENTRY = S_BASE
  module type S =
  sig
    module Entry : ENTRY
    type 'state t = 'state -> Entry.t -> bool
  end
  module Make(Entry:ENTRY) : S with module Entry = Entry =
  struct
    module Entry = Entry
    type 'state t = 'state -> Entry.t -> bool
    module Stateless =
    struct
        let make ~(f:Entry.t -> bool) : unit t = fun (_:'state) -> f
    end
  end
end

module type S =
sig
  include S_BASE
  module Predicate : Predicate.S with
    module Entry.Events = Events and
    module Entry.Data = Data
end

module Make (Events : EVENTS) (Data : Data.S) :
    S with
    module Events = Events and
    module Data = Data =
struct
  module I =
  struct
    type t = {events:Events.t;data:Data.t} [@@deriving make, show]
    module Events = Events
    module Data = Data
    module Or_error = Or_error
    let data t = t.data
    let events t = t.events
  end
  include I
  module Predicate = Predicate.Make(I)
end

