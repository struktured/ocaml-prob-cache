open Prob_cache.Std

module type EVENT = Riak_model_intf.EVENT
module type EVENTS = Riak_model_intf.EVENTS

module type S = Riak_model_intf.S

module Make(Event:EVENT) =
  Riak_model_impl.Set.Make(Event)
