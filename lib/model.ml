open Core.Std
open Mpmw
open Bin_prot
open Bin_prot.Std
open Bin_prot_utils
(* TODO move into some other package *)
let rec powerset = function
  | [] -> [[]]
  | h::t -> List.fold_left (powerset t) ~f:(fun xs t -> ((h::t)::t::xs)) ~init:[] ;;

type 'a events = 'a list with bin_io

type value = int with bin_io

type 'a t = {name:string;cache:(('a events), value) Cache.t}

let create name key_serializer =
  let value_serializer = Bin_prot_utils.create bin_read_value bin_write_value in
  let cache = Cache.create 
    ~key_serializer
    ~value_serializer 
    ~bucket:name 
    () in {name;cache}

let count ~(model:'a t) ~events = 
  match_lwt Cache.get model.cache events with
  Some x -> Lwt.return x
  | None -> Lwt.return 0

let conditional_prob ?(conditioned_on=[]) ~model ~events =
  lwt event_count = count model events in
  lwt given_cond_count = count model conditioned_on in
  lwt cond_count = if given_cond_count = 0 && (not (List.is_empty conditioned_on)) then count model [] else Lwt.return(given_cond_count) in
  if (cond_count = 0) then Lwt.return(Float.of_int 0) else
  Lwt.return((Float.of_int event_count) /. (Float.of_int cond_count)) 

let increment ~(model:'a t) ?(weight=1) ~(events:'a events) =
  lwt cnt = (count model events) in 
  Cache.put model.cache events (cnt + weight)

let observe ~(model:'a t) ~(events:'a events) =
  List.iter (powerset events) (fun set -> Lwt.ignore_result (increment model set));
 
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


   
