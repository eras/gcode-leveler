let optimize ?max_steps ~epsilon x0 cost step =
  let rec loop cur_step prev_cost x =
    if (match max_steps with
	  | Some max_steps when cur_step >= max_steps -> false
	  | _ -> true)
    then
      let new_cost = cost x in
	Printf.printf "cost difference %f\n" (new_cost -. prev_cost);
	if abs_float (new_cost -. prev_cost) < epsilon 
	 then 
	   let _ = Printf.printf "found at step %d\n" cur_step in
	     x
	else loop (cur_step + 1) new_cost (step cur_step x)
    else
      x
  in
    loop 0 (cost x0) (step 0 x0)

let derivate ~epsilon f x = (f (x +. epsilon) -. f (x -. epsilon)) /. (2.0 *. epsilon)

let gradient2 ~epsilon f1 f2 (x1, x2) = 
  let e1, e2 = epsilon in
    (derivate e1 f1 x1, derivate e2 f2 x2)

