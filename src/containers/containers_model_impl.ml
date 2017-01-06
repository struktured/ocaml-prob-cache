open Prob_cache.Std
open Or_errors.Std
open Or_errors_containers.Std
module Float = CCFloat
module Model_intf = Containers_model_intf
(** Represents a single event- must be comparable and showable *)
module type EVENT = Model_intf.EVENT

(** Represents an abstract collection of events *)
module type EVENTS = Model_intf.EVENTS

(** A module type provided polymorphic probability model caches. 
    Uses in memory models backed by the containers api *)

module type S = Model_intf.S
module type S_KERNEL = Model_intf.S_KERNEL
module Data = Model_intf.Data
module Fun = CCFun

module Make (Events:EVENTS) (Or_error:OR_ERROR) =
  (*S with module Events = Events and module Or_error = Or_error =*)
struct
  module Cache = CCMap.Make(Events)
  module Int = CCInt
  module T =
  struct
    module Events = Events
    type prior_count = Events.t -> int
    type prior_exp = Events.t -> float
    module Or_error = Model_intf.Or_error
    module Data = Data
    type update_rule = Events.t Data.update_rule and
      t = {
        name : string;
        cache : Data.t Cache.t;
        prior_count : prior_count;
        prior_exp : prior_exp;
        update_rule : update_rule }
    let default_prior_count (e:Events.t) = 0

  let default_prior_exp (e:Events.t) = 0.0

  let default_update_rule : update_rule = Update_rules.mean
  end

  module Create_fun : CREATE_FUN with
    type t = T.t and
    module Events = T.Events and
    module Data = T.Data and
    module Or_error = T.Or_error =
  struct
    include T
    module Options =
    struct
      module Cache_name = Prob_cache_options.Cache_name
      module Prior =  Prob_cache_prior.Make(Events)(Data)
      module Update = Prob_cache_update_rule_option.Make(Events)(Data)
      class options(name:string)(prior:Prior.t)(update:Update.t) =
      object
          inherit Prob_cache_options.Traits.cache_name(name)
          inherit Prior.prior(prior#prior_exp)(prior#prior_count)
          inherit Update.update(update#update)
      end
      type t = options
      let default = new options Cache_name.default#cache_name Prior.default Update.default
    end
    let with_cache ?(opt=Options.default) (*
        ?(update_rule=default_update_rule)
        ?(prior_count=default_prior_count)
        ?(prior_exp=default_prior_exp)
        ~(name:string) *) f = f
          {name=opt#cache_name;
           cache=Cache.empty;
           prior_count=opt#prior_count;
           prior_exp=opt#prior_exp;
           update_rule=opt#update}
    let name t = t.name
    let update_rule t = t.update_rule
  end

  module Data_fun : DATA_FUN with
    type t = T.t and
    module Events = T.Events and
    module Data = T.Data and
    module Or_error = T.Or_error =
  struct
    include T
    let data (events:Events.t) (t:t) : Data.t Or_error.t =
      match Cache.get events t.cache with
        | Some data -> Or_error.return data
        | None -> Or_error.return Data.empty
  end

  module Observe_data_fun : OBSERVE_DATA_FUN with
   type t = T.t and
   module Events = T.Events and
   module Data = T.Data and
   module Or_error = T.Or_error =
  struct
    include T
  let _observe_data data (events:Events.t) (t:t) =
    let orig_opt = Cache.get events t.cache in
    let data = CCOpt.maybe (fun orig ->
      Data.join ~obs:events ~update_rule:t.update_rule orig data) data orig_opt in
    {t with cache=Cache.add events data t.cache}

  let observe_data data (events:Events.t) (t:t) = Or_error.return @@
    CCList.fold_right (fun l t -> _observe_data data l t) (Events.subsets events) t
  end

  module Fold_fun : FOLD_FUN with
    type t = T.t and
    module Entry.Events = T.Events and
    module Entry.Data = T.Data and
    module Or_error = T.Or_error =
  struct
    include T
    module Entry : Entry.S with
      module Events = Events and module Data = Data =
    struct
      module I = Entry.Make(Events)(Data)
      module Events = Events
      module Data = Data
      include (I : module type of I with
        module Events := Events and
        module Data := Data)
    end
    module Fold : Fold.S with
      module Entry = Entry  =
    struct
      module I = Fold.Make(Entry)
      module Entry = Entry
      include (I : module type of I with
        module Entry := Entry)
      let compare _ = failwith("nyi")
    end
    let fold _ = failwith("nyi")
  end
  module Model_kernel =
  struct
    module K = Model_kernel.Make
    (Events)
    (Create_fun)
    (Data_fun)
    (Observe_data_fun)
    (Fold_fun)
    module Events = Events
    module Data = Data
    include (K:module type of K with module Events := Events and module Data := Data)
  end

  module Decorated = Model_decorator.Make(Model_kernel)
  module Events = Events
  module Event = Events.Event
  module Data = Data
  module Or_error = Model_intf.Or_error
  include (Decorated :
            module type of Decorated with
            module Events := Events and
           (* module Event := Event and*)
            module Data := Data and
            module Or_error := Or_error)
end

module Set =
struct
module Make(Event:EVENT) : EVENTS with module Event = Event =
struct
  module Multiset =
    struct
       module Event = Event
       include CCMultiSet.Make(Event)
       let show t = to_list t |> List.map Event.show |> String.concat " & "
       let pp formatter t = Format.fprintf formatter "%s" (show t)
       let join = union
       let subsets t = List.map of_list (Prob_cache_powerset.generate (to_list t))
       let filter f l = to_list l |> CCList.filter f |> of_list
       let fold f t acc = to_list t |> fun t -> CCList.fold_right f t acc
       let iter f t = iter t @@ fun _ e -> f e 
     end
  include Multiset
  module Events_multiset = Events.Make(Multiset)
  include (Events_multiset :
    module type of Events_multiset with module Event := Event)
end
end

module Sequence =
struct
module Make(Event:EVENT) : EVENTS with module Event = Event =
struct
  module OrdList =
    struct
      module Event = Event
      type t = Event.t list [@@deriving ord]
      let of_list l = l
      let to_list l = l
      let join = CCList.append
      let empty = CCList.empty
      let subsets (l:t) = let (accum, _) =
        List.fold_left
         (fun ((accum: t list), (l:t)) e -> let l' = l@[e] in (l'::accum, l'))
         ([], []) l in []::accum
     let iter  = List.iter
     let fold = CCList.fold_right
     let filter = CCList.filter
     let remove (t:t) x = CCList.remove ~x t
     let add t e = join t [e]
  let is_empty t = empty = t
  let show t = t |> List.map Event.show |> String.concat " & "
  let pp formatter t = Format.fprintf formatter "%s" (show t)
    end
  include OrdList
  module Events = Events.Make(OrdList)
  include (Events : module type of Events with module Event := Event)
end
end
