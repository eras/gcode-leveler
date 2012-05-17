let epsilon = 0.0000001;;

let p x y = ([|x|], y);;

let training = [|p 1.0 0.0; p 0.0 1.0; p ~-.1.0 2.0|];;

let _ = 
  let theta = Optimize.linreg ~max_steps:10 ~epsilon 1.0 [|0.0; 0.0|] training in
    Array.iter (Printf.printf "%f ") theta;
    Printf.printf "\n"

let p2 x1 x2 y = ([|x1; x2|], y);;

let training2 = [|p2 1.0 ~-.0.1 0.0; 
		  p2 0.0 0.0 1.0; 
		  p2 ~-.1.0 0.1 2.0|];;

let _ = 
  let theta = Optimize.linreg ~max_steps:10 ~epsilon 1.0 [|0.0; 0.0; 0.0|] training2 in
    Array.iter (Printf.printf "%f ") theta;
    Printf.printf "\n"

(* 0.100000: 118.332092 28.918486 48.072728 82.298368 *)
(* 0.300000: 113.345793 13.885774 44.496202 79.221698 *)

let training3 = [|p2 118.3 28.918486 0.1;
		  p2 113.3 13.885774 0.3|];;

let _ = 
  let theta = Optimize.linreg ~max_steps:50 ~epsilon 0.001 [|10.0; 10.0; 10.0|] training3 in
    Array.iter (Printf.printf "%f ") theta;
    Printf.printf "\n"

