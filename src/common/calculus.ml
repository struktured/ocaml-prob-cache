open Events_common

module Make(Model:Model_common.S) = struct
module Events = Model.Events
module Data = Model.Data
module Result = Model.Result
module Float = CCFloat

module Derived =
struct
  type t = {prob:float; data: Data.t option} [@@derving show]

  let or_error ?(prob=1.0) data = match data with 
    | Result.Ok data -> Result.Ok {prob;data}
    | Result.Error _ as e -> e
  let create ?(prob=1.0) data = {prob;data}
  let data t = t.data
  let prob t = t.prob

  let data_or_none = CCOpt.flat_map data
  let prob_or ?(value=0.) t = CCOpt.maybe prob value t

end

type t = And of Events.t | Or of t * t | Not of t | Given of t * t [@@deriving show]
let _and t t' = And (Events.of_list [t;t'])
let _or t t' = Or (t, t')
let _not t = Not t
let _given t t' = Given (t, t')

let rec eval (m:Model.t) (t:t) =
  let open Model.Or_error.Monad_infix in
  let module M = Model.Or_errors in
  match t with
| And e -> Derived.or_error (M.data e m)
| Or (t1, t2) ->
    eval m t1 >>= fun d1_opt ->
    eval m t2 >>| fun d2_opt ->
    begin
      match (Derived.data d1_opt, Derived.data d2_opt) with
    | (Some d1, Some d2) -> Derived.create (Some (Data.join (Model.update_rule m) d1 d2))
    | (None, Some d2) -> d2_opt
    | (Some d1, None) -> d1_opt
    | (None, None) -> Derived.create None
    end
| Not t ->
    begin match t with
  | Not t' -> eval m t'
  | And e -> M.complement e m >>= fun complement -> eval m (And complement)
  | Or (e, e') -> eval m (Or (Not e, Not e'))
  | Given (t1, t2) -> eval m (Given (Not t1, Not t2))
    end
| Given (t1, t2) ->
    match t1,t2 with
    | And e1, And e2 ->
      let joined = Events.join e1 e2 in
      Model.data joined m >>= fun d_joined_opt ->
      CCOpt.map (fun d_joined ->
        let cond_count = Data.count d_joined in
    (* (a) If the conditional probability is zero then is so must been the whole probability 
       (b) If the conditional (possibly empty) events have been observed then we normalize by its frequency count 
     *)
    let prob =  
      if (cond_count = 0) then (Float.of_int 0) else
    let joined_events_count = count (Events.join cond events) t in
    (Float.of_int joined_events_count) /. (Float.of_int cond_count)



    begin
      let open Derived in
      match (derived1,derived2) with
    | ({prob=p1; data=Some d1}, {prob=p2; data=Some d2}) -> 
        let joined = 
    | ({prob=_; data=Some d1}, {prob=_; data=None}) -> Derived.create ~prob:0.0 None
    | ({prob=_; data=None}, {prob=_; data=Some d2}) -> Derived.create ~prob:0.0 None
    | ({prob=_; data=None}, {prob=_; data=None}) -> Derived.create ~prob:0.0 None
    end

module Infix =
struct
  let (&) = _and
  let (+) = _or
  let (!) = _not
  let (||) =_given
end
end


