open BatPervasives

open GcodeParser

module Lnoexn = BatList.Exceptionless

let ( **> ) a b = a b

let coalesce2 a b =
  match a with
    | None -> b
    | Some _ -> a

let app3 f (x, y, z) = (f x, f y, f z)

let distance2 (x1, y1) (x2, y2) = sqrt ((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0)

let distance2_opt a b =
  match a, b with
    | (Some x1, Some y1), (Some x2, Some y2) -> Some (distance2 (x1, y1) (x2, y2))
    | (Some x1, None), (Some x2, None) -> Some (abs_float (x2 -. x1))
    | (None, Some y1), (None, Some y2) -> Some (abs_float (y2 -. y1))
    | _ -> None

let midway a b = (a +. b) /. 2.0

let midway_opt a b = 
  match a, b with
    | Some a, Some b -> Some (midway a b)
    | _ -> None

let midway_goto a b =
  let x = midway_opt a.x b.x in
  let y = midway_opt a.y b.y in
  let z = midway_opt a.z b.z in
  let e = midway_opt a.e b.e in
    { x; y; z; e; rest = b.rest }

let rec interpolate_absolute threshold g1_0 g1_2 =
  if BatOption.default false (BatOption.map ((<) threshold) (distance2_opt (g1_0.x, g1_0.y) (g1_2.x, g1_2.y))) 
  then 
    let g1_1 = midway_goto g1_0 g1_2 in
    let g1_2 = { g1_2 with rest = "\n" } in
      interpolate_absolute threshold g1_0 g1_1 @ interpolate_absolute threshold g1_1 g1_2
  else (
    [g1_2]
  )

let rec interpolate_relative threshold g1_1 =
  let origo = { x = Some 0.0; y = Some 0.0; z = Some 0.0; e = Some 0.0; rest = "\n" } in
    if BatOption.default false (BatOption.map ((<) threshold) (distance2_opt (origo.x, origo.y) (g1_1.x, g1_1.y)))
    then
      let g1_0 = midway_goto origo g1_1 in
      let g1_1 = { g1_0 with rest = "\n" } in
	interpolate_relative threshold g1_0 @ interpolate_relative threshold g1_1
    else (
      [g1_1]
    )
      
let g1_origo = { x = Some 0.0; y = Some 0.0; z = Some 0.0; e = Some 0.0; rest = "\n" }

let interpolate threshold data =
  let rec coords_of (mode, g1_0) current =
    match current with
      | None -> raise BatEnum.No_more_elements
      | Some ((Other _) as code) ->
	  ([code], (mode, g1_0))
      | Some (Move ((G0 | G1) as move, g1_2)) ->
	  let map_ofs = match mode with
	    | `Absolute -> coalesce2
	    | `Relative ->
		fun old new' ->
		  match old, new' with
		    | _, None -> None
		    | None, Some x -> Some x
		    | Some old, Some new' -> Some (new' +. old)
	  in
	  (List.map (fun x -> Move (move, x)) 
	     (match mode with
		| `Absolute ->
		    interpolate_absolute
		      threshold 
		      g1_0
		      g1_2
		| `Relative ->
		    interpolate_relative
		      threshold
		      g1_2
	     ),
	   (mode,
	    { x = map_ofs g1_2.x g1_0.x;
	      y = map_ofs g1_2.y g1_0.y;
	      e = map_ofs g1_2.e g1_0.e;
 	      z = map_ofs g1_2.z g1_0.z;
	      rest = "" }))
      | Some ((G90abs g) as code) -> ([code], (`Absolute, g1_0))
      | Some ((G91rel g) as code) -> ([code], (`Relative, g1_0))
      | Some ((G92 g) as code) -> ([code], (mode, g))
  in
    BatEnum.concat (
      BatEnum.from_loop 
	(`Absolute, { x = None; y = None; z = None; e = None; rest = "" })
	(fun (mode, prev) ->
	   let (list, (mode', prev')) = coords_of (mode, prev) (BatEnum.get data) in
	     (BatList.enum list, (mode', prev'))
	)
    )

let bump power dim x = (-. 1.0) /. (dim /. 2.0) ** power *. abs_float (x -. dim /. 2.0) ** power +. 1.0

let project_x f (x, _y, _z) = f x

let project_y f (_x, y, _z) = f y

let project_z f (_x, _y, z) = f z

let map_z f code =
    match code with
      | Move ((G0 | G1) as move, (({ x = Some x; y = Some y; z = Some z } as goto))) ->
	  Move (move, { goto with z = Some (f (x, y, z)) })
      | G92 ({ x = Some x; y = Some y; z = Some z } as goto) ->
	  G92 { goto with z = Some (f (x, y, z)) }
      | Move ((G0 | G1), _) | G92 _ -> failwith "Cannot perform mapping, not all X, Y and Z are known"
      | G90abs _ | G91rel _ | Other _ -> code

let interpolate1 (x_min, x_max) (y_min, y_max) x =
  (x -. x_min) /. (x_max -. x_min) *. (y_max -. y_min) +. y_min

let map_by_d ~offset ~points (x, y, z) =
  let adjust =
    if Array.length points = 0 
    then 0.0
    else
      let ds = Array.map (fun (x', y', z') -> 1.0 /. distance2 (x', y') (x, y) ** 2.0) points in
	try let (_, _, z) = points.(BatArray.findi (fun d ->  classify_float d = FP_infinite) ds) in z
	with Not_found ->
	  let ds'sum = Array.fold_left (+.) 0.0 ds in
	  let ws = BatArray.map2 (fun d (_, _, z) -> d *. z) ds points in
	    Array.fold_left (+.) 0.0 ws /. ds'sum
  in
    adjust +. z +. offset

type env = {
  step_size : float;
  mapping   : input -> input;
  x_dim	    : float;
  y_dim	    : float;
}

let transform { step_size; mapping } =
  let data = parse_gcode (Lexing.from_channel Pervasives.stdin) in
  let data = BatEnum.map mapping data in
  let data = interpolate step_size data in
  let data = 
    BatEnum.unfold 
      (`Absolute, None)
      (fun (mode, prev) ->
	   match BatEnum.get data with
	     | None -> None
	     | Some x -> 
		 let mode', prev' =
		   match x with
		     | Move ((G0 | G1), _) | G92 _ -> mode, Some x
		     | G90abs _ -> `Absolute, prev
		     | G91rel _ -> `Relative, prev
		     | Other _ -> mode, prev
		 in
		   Some (string_of_input ~mode ?previous:prev x, (mode', prev'))
      )
  in
    Printf.printf "; Mangled with gcode-leveler https://github.com/eras/gcode-leveler\n";
    BatEnum.iter print_string data

let show_table { x_dim; y_dim; mapping } =
  (y_dim, ~-.10.0) --. 1.0 |> BatEnum.iter **> fun y ->
    begin 
      (1.0, 10.0) --. x_dim |> BatEnum.iter **> fun x ->
	match mapping (Move (G1, { x = Some x; y = Some y; z = Some 0.0; e = None; rest = "" })) with
	  | Move (G1, { z = Some z }) ->
	      Printf.printf " % .03f" z
	  | _ -> assert false
    end;
    Printf.printf "\n"

let main () =
  let mode = ref transform in
  let points = ref [] in
  let offset = ref 0.0 in
  let x_dim = ref 160 in
  let y_dim = ref 199 in
  let step_size = ref 50.0 in
  let set_mode mode' = Arg.Unit (fun () -> mode := mode') in
  let add_point str = 
    match Pcre.split ~pat:"," str with
      | [x'str; y'str; z'str] ->
	  let xyz = app3 float_of_string (x'str, y'str, z'str) in
	    points := xyz::!points
      | _ ->
	  Printf.eprintf "Invalid number of coordinates for -p. Expected x,y,z\n"
  in
    Arg.parse [("-p", Arg.String add_point, "Add point x,y,z to the offset map");
	       ("-ofs", Arg.Set_float offset, "Set additional offset");
	       ("-step", Arg.Set_float step_size, "Set traveled distance that is interpolated");
	       ("-x", Arg.Set_int x_dim, "Set area X size");
	       ("-y", Arg.Set_int y_dim, "Set area Y size");
	       ("-table", set_mode show_table, "Show table of transformation at scale 1/10");
	      ] (fun arg -> Printf.ksprintf failwith "Invalid argument: %s\n%!" arg) "G-code leveler";
    let offset = !offset in
    let points = Array.of_list !points in
    let mapping = (map_z (map_by_d ~offset ~points)) in
    let x_dim, y_dim = float !x_dim, float !y_dim in
    let step_size = !step_size in
      !mode { step_size; mapping; x_dim; y_dim }

let _ = main ()
