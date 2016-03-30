open Prob_cache_common

module type EVENT = Model_intf.EVENT
module type EVENTS = Model_intf.EVENTS

module type S = Model_intf.S

module Make(Event:EVENT) = struct
  module EventSet = Model_impl.Make_event_set(Event)
  include Model_impl.Make_for_events(EventSet)
end
