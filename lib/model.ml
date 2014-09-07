open Mpmw
open Bin_prot
open Bin_prot.Std
type topic = string with bin_io
type 'a key = ELEMENT of ('a * topic) | TOPIC of topic with bin_io

type value = int with bin_io


type 'a t = (('a key), value) Cache.cache

let count (model:'a t) element = 
  match_lwt Cache.get model element with
  Some x -> Lwt.return x
  | None -> Lwt.return 0
 
let increment (model:'a t) (element:'a key) =
  lwt cnt = (count model element) in 
  Cache.put model element (cnt+1)

let observe (model:'a t) (element:'a) topic =
  Lwt.ignore_result (increment model (TOPIC topic));
  Lwt.ignore_result (increment model (ELEMENT (element, topic)))
 
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


   
