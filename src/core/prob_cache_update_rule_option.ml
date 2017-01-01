module type EVENTS = Prob_cache_events.EVENTS
module type DATA = Prob_cache_data.DATA
module Options = Prob_cache_options

module Update_fn = Prob_cache_update_rules.Update_fn
(** Update rule wrapped as an optional argument *)
module type S =
sig

  (** The module type representing a collection of events *)
  module Events : EVENTS

  (** Container for the descriptive statistics **)
  module Data : DATA

  class update : Events.t Update_fn.t ->
    object method update : Events.t Update_fn.t end
  type t = update

  include Options.S with type t := t
end

(** Creates a new prior signature given the events and data signatures *)
module Make(Events:EVENTS)(Data:DATA) :
  S with module Events := Events and module Data := Data =
struct
  (** The module type representing a collection of events *)
  module Events = Events

  (** Container for the descriptive statistics **)
  module Data = Data

  class update(f:Events.t Update_fn.t) = object method update = f end
  type t = update

  let default : t = new update(Prob_cache_update_rules.Mean.update)
end

