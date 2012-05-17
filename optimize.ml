open BatPervasives
open Utils

(* this is not gradient descent by itself, it depends
   on the cost and step functions *)
let optimize ?max_steps ?(min_steps=0) ~epsilon (x0:'a) (cost:'a -> float) (step:int -> 'a -> 'a) =
  let rec loop cur_step prev_cost x =
    if (match max_steps with
	  | Some max_steps when cur_step >= max_steps -> false
	  | _ -> true)
    then
      let new_cost = cost x in
	Printf.printf "cost difference %f\n" (new_cost -. prev_cost);
	if abs_float (new_cost -. prev_cost) < epsilon && cur_step >= min_steps
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

let linreg_hypo x theta = 
  let sum = ref theta.(0) in
    for c = 0 to Array.length x - 1 do
      sum := !sum +. theta.(c + 1) *. x.(c);
    done;
    !sum

let (!@) x = float (Array.length x)

let linreg_cost xs ys theta =
  assert (Array.length xs = Array.length ys);
  1.0 /. (2.0 *. !@ theta) *. sum (BatArray.map2 (fun x y -> (linreg_hypo x theta -. y) ** 2.0) xs ys)

let trace_exn label f a =
  try f a 
  with exn -> 
    Printf.printf "exception at %s\n" label;
    raise exn

let linreg_cost' xs ys theta =
  assert (Array.length xs = Array.length ys);
  let m = Array.length xs in
    Array.init (Array.length theta) **> fun j ->
      1.0 /. !@ theta *. sum (Array.init m **> fun i ->
				let x = xs.(i) in
				let y = ys.(i) in
				  (linreg_hypo x theta -. y) *. (if j = 0 then 1.0 else x.(j - 1))
			     )

let normalization xs =
  let mu = average xs in
  let mn = Array.map (fun x -> x -. mu) xs in
  let sigma = stddev mn in
  let normalize x = (x -. mu) /. sigma in
  let denormalize x = x *. sigma +. mu in
    (normalize, denormalize)

let transpose_array xs =
  Array.init (Array.length xs.(0)) **> fun x ->
    Array.init (Array.length xs) **> fun y ->
      xs.(y).(x)

let linreg ?max_steps ?min_steps ~epsilon alpha theta0 training =
  let xs = Array.map fst training in
  let xs' = transpose_array xs in
  let ns = Array.map normalization xs' in
  let xs' = BatArray.map2 (fun (n, _n') x -> Array.map n x) ns xs' in
  let xs = transpose_array xs' in
  let ys = Array.map snd training in
    assert (Array.length theta0 = Array.length (fst training.(0)) + 1);
    let theta = 
      optimize ?max_steps ?min_steps ~epsilon theta0
	(linreg_cost xs ys)
	(fun _nth_step theta ->
	   let step = Array.map (( *. ) alpha) **> linreg_cost' xs ys theta in
	     BatArray.map2 (-.) theta step
	)
    in
    let normalize_row xs'row = BatArray.map2 (fun (n, _n') x -> n x) ns xs'row in
      (theta, normalize_row)
      

