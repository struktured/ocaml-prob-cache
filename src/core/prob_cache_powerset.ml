let rec generate = function
  | [] -> [[]]
  | h::t -> List.fold_left (fun xs t -> ((h::t)::t::xs)) [] (generate t) ;


