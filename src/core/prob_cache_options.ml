module type S =
sig
  type t
  val cache_name : t -> string
end

class options(cache_name:string) =
  object(self)
    method cache_name = cache_name
  end

module Extensible : S with type t = options =
struct
 type t = options
  let cache_name t = t#cache_name
end

include Extensible
