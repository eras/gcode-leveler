open BatPervasives

module Lnoexn = BatList.Exceptionless

type g1 =
    { x : float option;
      y : float option;
      z : float option;
      e : float option;
      rest : string; }

type rest = string

type input = 
  | G1 of g1			     (* go to point *)
  | G90abs of rest		     (* switch to absolute movement *)
  | G91rel of rest		     (* switch to relative movement *)
  | G92 of g1			     (* set values *)
  | Other of string

let string_of_token = function
  | Lexer.Eof -> ""
  | Lexer.Entry (register, Lexer.Int value) -> Printf.sprintf "%c%d" register value
  | Lexer.Entry (register, Lexer.Float value) -> Printf.sprintf "%c%.4f" register value
  | Lexer.Comment str -> str
  | Lexer.Eol -> "\n"

let coalesce2 a b =
  match a with
    | None -> b
    | Some _ -> a

let app3 f (x, y, z) = (f x, f y, f z)

let app4 f (x, y, z, e) = (f x, f y, f z, f e)

let zip4 (a1, a2, a3, a4) (b1, b2, b3, b4) = ((a1, b1), (a2, b2), (a3, b3), (a4, b4))

let parse_gcode () =
  let lex_input = Lexing.from_channel Pervasives.stdin in
  let next = ref None in
  let rec eof () =
    next := Some eof;
    raise BatEnum.No_more_elements;
  in
  let mode = ref `Absolute in
  let prev_at = (ref None, ref None, ref None, ref None) in
  let process accu =
    let get_float x = 
      match Lnoexn.find (function Lexer.Entry (reg, _) when reg = x -> true | _ -> false) accu with
	| None -> None
	| Some (Lexer.Entry (_, Lexer.Float value)) -> Some value
	| Some (Lexer.Entry (_, Lexer.Int value)) -> Some (float_of_int value)
	| Some _ -> assert false
    in
    let g1 = List.mem (Lexer.Entry ('G', Lexer.Int 1)) accu in
    let g90 = List.mem (Lexer.Entry ('G', Lexer.Int 90)) accu in
    let g91 = List.mem (Lexer.Entry ('G', Lexer.Int 91)) accu in
    let g92 = List.mem (Lexer.Entry ('G', Lexer.Int 92)) accu in
    let (x, y, z, e) = app4 get_float ('X', 'Y', 'Z', 'E') in
    let rest = 
      lazy (
	let r = List.filter (function Lexer.Entry (reg, _) when List.mem reg ['X'; 'Y'; 'Z'; 'G'; 'E'; 'M'] -> false | _ -> true) accu in
	  String.concat "" **> List.rev_map string_of_token r
      ) in
    let default_zero at = app4 (BatOption.default 0.0) at in
    let new_at = 
      match !mode with
	| `Absolute -> app4 (uncurry coalesce2) (zip4 (x, y, z, e) (app4 (!) prev_at))
	| `Relative -> 
	    app4 
	      (fun x -> Some x)
	      (default_zero (x, y, z, e))
    in
    let update_positions () = 
      ignore (app4 (uncurry (:=)) 
		(zip4 prev_at
		   (match !mode with
		      | `Absolute ->  new_at
		      | `Relative -> app4 (fun x -> Some x) (app4 (uncurry (+.)) (zip4 (default_zero new_at) (default_zero (app4 (!) prev_at))))
		   )
		)
	     )
    in
    let value =
      match 0 with
	| _ when g90 -> 
	    mode := `Absolute;
	    G90abs (Lazy.force rest)
	| _ when g91 ->
	    mode := `Relative;
	    G91rel (Lazy.force rest)
	| _ when g1 ->
	    let (x, y, z, e) = new_at in
	      update_positions ();
	      (G1 { x; y; z; e; rest = Lazy.force rest })
	| _ when g92 ->
	    let (x, y, z, e) = new_at in
	      update_positions ();
	      G92 { x; y; z; e; rest = Lazy.force rest }
	| _ -> 
	    Other (String.concat " " (List.rev_map string_of_token accu))
    in
      value
  in
  let rec loop accu =
    match !next with
      | Some fn ->
	  next := None;
	  fn ()
      | None ->
	  match Lexer.token lex_input with
	    | Lexer.Eof -> 
		next := Some eof;
		process accu
	    | Lexer.Entry _ as entry ->
		loop (entry::accu)
	    | Lexer.Comment _ as token ->
		next := Some (fun () -> Other (string_of_token token));
		process accu
	    | Lexer.Eol ->
		process (Lexer.Eol::accu)
  in
    BatEnum.from (fun () -> loop [])

let string_of_input ?(mode=`Absolute) ?(previous) = 
  let (x', y', z', e') = 
    match mode, previous with
      | `Absolute, Some (G1 { x; y; z; e; rest }) -> (x, y, z, e)
      | `Absolute, Some (G92 { x; y; z; e; rest }) -> (x, y, z, e)
      | (`Absolute | `Relative), Some (G90abs _ | G91rel _ | Other _) 
      | (`Absolute | `Relative), None
      | `Relative, _ -> (None, None, None, None)
  in
  let coord_cmd label x y z e rest =
    let f label x x' = 
      match mode with
	| `Absolute ->
	    ( match x', x with
		| _, None -> ""
		| Some x', Some x when x = x' -> ""
		| _, Some x -> Printf.sprintf " %s%.4f" label x )
	| `Relative ->
	    match x with
	      | Some x when x <> 0.0 -> Printf.sprintf " %s%.4f" label x
	      | None | Some _ -> ""
    in
      label ^ f "X" x x' ^ f "Y" y y' ^ f "Z" z z' ^ f "E" e e' ^ " " ^ rest
  in
  function
  | G1 { x; y; z; e; rest } -> coord_cmd "G1" x y z e rest
  | G92 { x; y; z; e; rest } -> coord_cmd "G92" x y z e rest
  | G90abs rest -> "G90 " ^ rest
  | G91rel rest -> "G91 " ^ rest
  | Other str -> str

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

let midway_g1 a b =
  let x = midway_opt a.x b.x in
  let y = midway_opt a.y b.y in
  let z = midway_opt a.z b.z in
  let e = midway_opt a.e b.e in
    { x; y; z; e; rest = b.rest }

let rec interpolate_absolute threshold g1_0 g1_2 =
  if BatOption.default false (BatOption.map ((<) threshold) (distance2_opt (g1_0.x, g1_0.y) (g1_2.x, g1_2.y))) 
  then 
    let g1_1 = midway_g1 g1_0 g1_2 in
    let g1_2 = { g1_2 with rest = "\n" } in
      interpolate_absolute threshold g1_0 g1_1 @ interpolate_absolute threshold g1_1 g1_2
  else (
    [g1_2]
  )

let rec interpolate_relative threshold g1_1 =
  let origo = { x = Some 0.0; y = Some 0.0; z = Some 0.0; e = Some 0.0; rest = "\n" } in
    if BatOption.default false (BatOption.map ((<) threshold) (distance2_opt (origo.x, origo.y) (g1_1.x, g1_1.y)))
    then
      let g1_0 = midway_g1 origo g1_1 in
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
      | Some ((G1 g1_2)) ->
	  let map_ofs = match mode with
	    | `Absolute -> coalesce2
	    | `Relative ->
		fun old new' ->
		  match old, new' with
		    | _, None -> None
		    | None, Some x -> Some x
		    | Some old, Some new' -> Some (new' +. old)
	  in
	  (List.map (fun x -> G1 x) 
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
      | G1 ({ x = Some x; y = Some y; z = Some z } as g1) ->
	  G1 { g1 with z = Some (f (x, y, z)) }
      | _ -> code

let interpolate1 (x_min, x_max) (y_min, y_max) x =
  (x -. x_min) /. (x_max -. x_min) *. (y_max -. y_min) +. y_min

let map_by_d ~offset ~points (x, y, z) =
  let adjust =
    if Array.length points = 0 
    then z +. offset 
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
  let data = parse_gcode () in
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
		     | G1 _ | G92 _ -> mode, Some x
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
	match mapping (G1 { x = Some x; y = Some y; z = Some 0.0; e = None; rest = "" }) with
	  | G1 { z = Some z } ->
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
