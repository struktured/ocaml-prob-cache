

let distance comment comment' (discussions: Discussion.t list) =
   let numerator = List.fold_left (fun sum discussion -> 
      let d_c_count = Model.count ~set:discussion ~element:comment in
      let d_c'_count = Model.count ~set:discussion ~element:comment' in
      let d_norm = Model.count ~set:discussion in
      let d_c_c'_mse = let diff = (d_c_count - d_c'_count) in 
      let new_sum = sum + (diff*diff) / (d_norm*d_norm) in new_sum) 0 discussions in
   let denom = 1 in 
   numerator / denom



   
