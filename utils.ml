let sum xs = Array.fold_left (+.) 0.0 xs 

let sum_list xs = List.fold_left (+.) 0.0 xs 

let minimum xs = Array.fold_left min infinity xs

let maximum xs = Array.fold_left max neg_infinity xs

let average xs = sum xs /. float (Array.length xs) 

let average_list xs = sum_list xs /. float (List.length xs) 

let clamp x'min x'max x = min x'max (max x'min x)

let timing label f a0 =
  let t0 = Unix.times () in
  let v = f a0 in
  let t1 = Unix.times () in
    Printf.printf "%s %f\n%!" label Unix.(t1.tms_utime -. t0.tms_utime);
    v
