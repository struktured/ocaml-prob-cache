module Data = Prob_cache_data
module type DATA = Data.S
module type EVENTS = Prob_cache_events.EVENTS
module type S =
sig
type t
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

module Cache_name : S with type t = Traits.cache_name =
struct
  class options = Traits.cache_name
  type t = Traits.cache_name
  let default = new options ("default-cache")
  end

module Port : S with type t = Traits.port =
struct
class options = Traits.port
type t = options
  let default = new options 0
  end
