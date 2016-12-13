module Data = Prob_cache_data
module type DATA = Data.S
module type EVENTS = Prob_cache_events.EVENTS
module type S =
sig
(*class options: string ->
object
method cache_name : string
end *)
type t  (* = options*)
val default : t

end

module Traits =
struct
class cache_name(cache_name:string) =
  object(self)
    method cache_name = cache_name
  end
class port(port:int) =
  object(self)
    method port = port
  end

end
(*
module Extensible :
  S with type t = options_cache_name =
struct
 class options = options_cache_name
 type t = options
let default = new options("default-cache")
end
*)
module Options_with_prior =
struct
module Make_internal(Events:EVENTS)(Data:DATA) =
struct
  (** The module type representing a collection of events *)
  module Events = Events

  (** Container for the descriptive statistics **)
  module Data = Data

  (** Defines a prior function in terms of counts with the observed events as input. *)
  type prior_count = Events.t -> int

  (** Define a prior function in terms of real values with the observed events as input. *)
  type prior_exp = Events.t -> float
  module Traits =
  struct
    include Traits
    class prior(prior_exp:prior_exp) (prior_count:prior_count) =
    object(self)
      method prior_exp = prior_exp
      method prior_count = prior_count
    end
  end
  class options(cache_name:string)(prior_exp:prior_exp)(prior_count:prior_count) =
      object(self)
          inherit Traits.cache_name(cache_name)
          inherit Traits.prior(prior_exp)(prior_count)
      end
  type t = options
  let default = new options ("default-cache") (fun _ -> 0.0) (fun _ -> 1)
  end
module Make(Events:EVENTS)(Data:DATA) =
struct
  module M = Make_internal(Events)(Data)
  include (M: S with type t = M.options)
end
end

include Options_with_prior

