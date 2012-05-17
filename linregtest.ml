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

