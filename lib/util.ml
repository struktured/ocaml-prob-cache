let rec powerset = function
  | [] -> [[]]
  | h::t -> List.fold_left (fun xs t -> ((h::t)::t::xs)) [] (powerset t) ;


