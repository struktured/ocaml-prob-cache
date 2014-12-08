
module type EVENT = Model_intf.EVENT

module type EVENTS = Model_intf.EVENTS

module type S = Model_intf.S

module Make(Event:EVENT) = Model_intf.Make_for_events(Model_intf.Make_event_sequence(Event))

