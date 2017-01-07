open Prob_cache.Std

module type EVENT = Riak_model_intf.EVENT
module type EVENTS = Riak_model_intf.EVENTS

module type S = Riak_model_intf.S

(*module Make(Event:EVENT) =
  Riak_model_impl.Set.Make(Event)
*)

module Make(Event:EVENT) : S with module Events.Event = Event =
struct
  module Events : EVENTS with module Event = Event =
  struct
    module Event = Event
    module M = Riak_model_impl.Set.Make(Event)
    include (M : module type of M with module Event := Event)
  end
  module Event = Events.Event
  module Or_error = Riak_model_intf.Or_error
  module M = Riak_model_impl.Make(Events)
  include (M : module type of M with
          module Events := Events and
          module Or_error := Or_error and
          module Event := Event)
end

