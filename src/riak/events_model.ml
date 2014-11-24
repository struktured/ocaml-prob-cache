open Prob_cache_common
module OldList = List
open Core.Std
module CoreList = List
module List = OldList

open Async.Std

module type EVENT = sig type t [@@deriving protobuf] end 

module type EVENTS = 
sig
  module Event : EVENT
  include Protobuf_capable.S 
  val is_empty : t -> bool
  val empty : t
  val union : t -> t -> t
  val of_list : Event.t list -> t
  val to_list : t -> Event.t list
end

module Int = Protobuf_capables.Int


module type S =
sig
  module Events : EVENTS
  module Event : module type of Events.Event
  module Cache : module type of Cache.Make(Events)(Int)
  type t
  val count : Events.t -> t -> (int, [> Opts.Get.error]) Deferred.Result.t  
  val create : string -> Conn.t -> t
 val observe : ?cnt:int -> Events.t -> t -> (('a Cache.Robj.t * Events.t Riakc.Cache.Option.t) list,
            [> Opts.Get.error ]) Deferred.Result.t
  val prob : ?cond:Events.t -> Events.t -> t -> (float, [> Opts.Get.error]) Deferred.Result.t  
  val name : t -> string
end


module Make_for_events (Events:EVENTS) : S with module Events = Events =
struct
  module Events = Events
  module Event = Events.Event
  module Int = Riakc.Cache.Int
  module Cache = Riakc.Cache.Make(Events)(Int)


  type t = {cache : Cache.t}

  let create bucket conn = {cache=Cache.create ~conn ~bucket}

  let count (events:Events.t) t : (int, [> Opts.Get.error]) Result.t Deferred.t  = 
  let open Cache.Robj in Cache.get t.cache events >>| function 
    | Ok robj -> 
        (match robj.contents with  
      | [] -> Ok 0 
      | [c] -> Ok (Cache.Robj.Content.value c)
      | l -> failwith "should only be one content value")
    | Error e -> Error e



(* Computes P(events|conditioned_on) via P(events, conditioned_on) / P(conditioned_on). If conditioned_on is blank it simply becomes the marginal probability P(events) *)
  let prob ?(cond=Events.empty) (events:Events.t) t : (Core.Std.Float.t, [> Opts.Get.error]) Result.t Deferred.t  =
    let open Deferred.Result.Monad_infix in 
    count (Events.union events cond) t >>= 
    fun event_count -> count cond t >>= 
    fun given_cond_count -> (if given_cond_count = 0 && (not (Events.is_empty cond)) 
                             then count Events.empty t else Deferred.return (Ok given_cond_count))
    >>| fun cond_count -> if (cond_count = 0) then Float.of_int 0 else
      (Float.of_int event_count) /. (Float.of_int cond_count)


  let increment ?(cnt=1) (events:Events.t) t =
    let open Deferred.Result.Monad_infix in
    count events t >>=
    fun event_count -> Cache.put t.cache ~k:events (Cache.Robj.of_value cnt) 

  let observe ?(cnt=1) (events:Events.t) t =
    let ps = Util.powerset (Events.to_list events) in
    Deferred.Result.all 
    (CoreList.map ps ~f:(fun set -> ((increment ~cnt (Events.of_list set) t))))

  let name t = Cache.get_bucket t.cache
end


module Make_events(Event:EVENT) : EVENTS with module Event = Event = 
struct
  module Event = Event
  module Hashset = Containers_misc.Hashset
  type t = Event.t Hashset.t

  let to_list t = CCSequence.to_list (Hashset.to_seq t)
  let of_list l = 
    let h = Hashset.empty (List.length l) in 
    CoreList.iter ~f:(fun e -> Hashset.add h e) l;h 

  type event_list = Event.t list [@@deriving protobuf]

  let union (t:t) (t':t) : t = Hashset.union t t'
  let empty = of_list []
  let is_empty t = Hashset.cardinal t = 0

  let to_protobuf t e =  event_list_to_protobuf (to_list t) e
  let from_protobuf d = of_list (event_list_from_protobuf d)
end


module Make(Event:EVENT) = Make_for_events(Make_events(Event))

