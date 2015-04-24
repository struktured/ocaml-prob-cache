
module type EVENT = Model_intf.EVENT

module type EVENTS = Model_intf.EVENTS

module type S = Model_intf.S

module Make(Event:EVENT) = Model_impl.Make_for_events
  (Model_impl.Make_event_sequence(Event))

