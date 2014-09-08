open Core.Std
open Mpmw
open Bin_prot
open Bin_prot.Std
open Bin_prot_utils
open Sexplib
open Sexplib.Std


(* TODO move into some other package *)
let rec powerset = function
  | [] -> [[]]
  | h::t -> List.fold_left (powerset t) ~f:(fun xs t -> ((h::t)::t::xs)) ~init:[] ;;

module type EVENT = sig type t with bin_io, compare, sexp end 

module Events = functor (Event:EVENT) ->
struct 
  module EventSet = Core.Std.Set.Make_binable(Event)
  include EventSet
end


module EventCache = functor (Event:EVENT) ->
  struct
type value = int with bin_io, compare, sexp

module Events = Events(Event)

type t = {name:string;cache:(Events.t, value) Cache.t}

let create name =
  let key_serializer = Bin_prot_utils.create Events.bin_read_t Events.bin_write_t in
  let value_serializer = Bin_prot_utils.create bin_read_value bin_write_value in
  let cache = Cache.create 
    ~key_serializer
    ~value_serializer 
    ~bucket:name 
    () in {name;cache}

let count ~model ~events = 
  match_lwt Cache.get model.cache events with
  Some x -> Lwt.return x
  | None -> Lwt.return 0

(* Computes P(events|conditioned_on) via P(events, conditioned_on) / P(conditioned_on). If conditioned_on is blank it simply becomes the marginal probility P(events) *)
let probability ?(conditioned_on=Events.empty) ~model ~(events:Events.t) =
  lwt event_count = count model (Events.union events conditioned_on) in
  lwt given_cond_count = count model conditioned_on in 
  lwt cond_count = if given_cond_count = 0 && 
    (not (Events.is_empty conditioned_on)) then count ~model ~events:Events.empty else Lwt.return(given_cond_count) in
  if (cond_count = 0) then Lwt.return(Float.of_int 0) else
  Lwt.return((Float.of_int event_count) /. (Float.of_int cond_count)) 

let increment ~model ?(weight=1) ~(events:Events.t) =
  lwt cnt = (count model events) in 
  Cache.put model.cache events (cnt + weight)

let observe ~model ~(events:Events.t) =
  List.iter (powerset (Events.to_list events)) 
    (fun set -> Lwt.ignore_result (increment model (Events.of_list set)));
end

  (* 
  let distance elem elem' model =
   let numerator = List.fold_left (fun sum discussion -> 
      let d_c_count = score set:discussion ~element:comment in
      let d_c'_count = score ~set:discussion ~element:comment' in
      let d_norm = count ~set:discussion ini
      let d_c_c'_mse = let diff = (d_c_count - d_c'_count) in 
     let new_sum = sum + (diff*diff) / (d_norm*d_norm) in new_sum) 0 discussions in
   let denom = 1 in 
   numerator / denom
*)


   
