open Core.Std
open Mpmw
open Bin_prot
open Bin_prot.Std

let rec powerset = function
  | [] -> [[]]
  | h::t -> List.fold_left (powerset t) ~f:(fun xs t -> ((h::t)::t::xs)) ~init:[] ;;

type 'a key = 'a list with bin_io

type value = int with bin_io

type 'a t = (('a key), value) Cache.t

let count (model:'a t) elements = 
  match_lwt Cache.get model elements with
  Some x -> Lwt.return x
  | None -> Lwt.return 0
 
let increment ~(model:'a t) ?(weight=1) ~(elements:'a key) =
  lwt cnt = (count model elements) in 
  Cache.put model elements (cnt + weight)

let observe (model:'a t) (elements:'a list) =
  List.iter (powerset elements) (fun set -> Lwt.ignore_result (increment model set));
 
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


   
