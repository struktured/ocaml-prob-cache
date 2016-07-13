
module type EVENT = Model_intf.EVENT

module type EVENTS = Model_intf.EVENTS

module type S = Model_intf.S

module Make(Event:EVENT) : S with module Events.Event = Event =
struct
  module Events : EVENTS with module Event = Event =
  struct
    module Event = Event
    module M = Model_impl.Sequence.Make(Event)
    include (M : module type of M with module Event := Event)
  end
  module Event = Events.Event
  module Or_error = Model_intf.Or_error
  module M = Model_impl.Make(Events)(Or_error)
  include (M : module type of M with
          module Events := Events and
          module Or_error := Or_error and
          module Event := Event)
end

