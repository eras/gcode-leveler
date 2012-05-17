open BatPervasives
open Utils

(* this is not gradient descent by itself, it depends
   on the cost and step functions *)
let optimize ?max_steps ~epsilon (x0:'a) (cost:'a -> float) (step:int -> 'a -> 'a) =
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

(* m = number of training examples
   x = training input
   y = output

 *)

let linreg_h2 x theta = 
  theta.(0) +. theta.(1) *. x.(0)

let (!@) x = float (Array.length x)

let linreg_cost xs ys theta =
  assert (Array.length xs = Array.length ys);
  1.0 /. (2.0 *. !@ theta) *. sum (BatArray.map2 (fun x y -> (linreg_h2 x theta -. y) ** 2.0) xs ys)

let linreg_cost2' xs ys theta =
  assert (Array.length theta = 2);
  assert (Array.length xs = Array.length ys);
  [|1.0 /. !@ theta *. sum (BatArray.map2 (fun x y -> linreg_h2 x theta -. y) xs ys);
    1.0 /. !@ theta *. sum (BatArray.map2 (fun x y -> (linreg_h2 x theta -. y) *. x.(0)) xs ys)|]

let linreg ?max_steps ~epsilon theta0 training =
  let xs = Array.map fst training in
  let ys = Array.map snd training in
    assert (Array.length theta0 = 2);	(* other theta sizes not supported yet *)
    assert (Array.length xs.(0) = 1); 	(* other sizes not supported yet *)
    optimize ?max_steps ~epsilon theta0
      (linreg_cost xs ys)
      (fun _nth_step theta ->
	 let step = linreg_cost2' xs ys theta in
	   BatArray.map2 (-.) theta step
      )
