open BatPervasives

module Lnoexn = BatList.Exceptionless

type g1 =
    { x : float option;
      y : float option;
      z : float option;
      e : float option;
      rest : string; }

type input = 
  | G1 of g1
  | M92 of g1
  | Other of string

let string_of_token = function
  | Lexer.Eof -> ""
  | Lexer.Entry (register, Lexer.Int value) -> Printf.sprintf "%c%d" register value
  | Lexer.Entry (register, Lexer.Float value) -> Printf.sprintf "%c%.4f" register value
  | Lexer.Comment str -> " ; " ^ str
  | Lexer.Eol -> "\n"

let coalesce2 a b =
  match a with
    | None -> b
    | Some _ -> a

let app4 f (x, y, z, e) = (f x, f y, f z, f e)

let zip4 (a1, a2, a3, a4) (b1, b2, b3, b4) = ((a1, b1), (a2, b2), (a3, b3), (a4, b4))

let parse_gcode () =
  let lex_input = Lexing.from_channel Pervasives.stdin in
  let next = ref None in
  let rec eof () =
    next := Some eof;
    raise BatEnum.No_more_elements;
  in
  let prev_at = (ref None, ref None, ref None, ref None) in
  let process accu =
    let f x = 
      match Lnoexn.find (function Lexer.Entry (reg, _) when reg = x -> true | _ -> false) accu with
	| None -> None
	| Some (Lexer.Entry (_, Lexer.Float value)) -> Some value
	| Some (Lexer.Entry (_, Lexer.Int value)) -> Some (float_of_int value)
	| Some _ -> assert false
    in
    let g1 = List.mem (Lexer.Entry ('G', Lexer.Int 1)) accu in
    let m92 = List.mem (Lexer.Entry ('M', Lexer.Int 92)) accu in
    let (x, y, z, e) = app4 f ('X', 'Y', 'Z', 'E') in
    let new_at = app4 (uncurry coalesce2) (zip4 (x, y, z, e) (app4 (!) prev_at)) in
    let rest = 
      lazy (
	let r = List.filter (function Lexer.Entry (reg, _) when List.mem reg ['X'; 'Y'; 'Z'; 'G'; 'E'; 'M'] -> false | _ -> true) accu in
	  String.concat "" **> List.rev_map string_of_token r
      ) in
    let update_positions () =
      ignore (app4 (uncurry (:=)) (zip4 prev_at new_at));
    in
    let value =
      if g1 && (x <> None || y <> None || z <> None || e <> None)
      then 
	let (x, y, z, e) = new_at in
	  update_positions ();
	  (G1 { x; y; z; e; rest = Lazy.force rest })
      else 
	if m92
	then (
	  let (x, y, z, e) = new_at in
	    update_positions ();
	    M92 { x; y; z; e; rest = Lazy.force rest }
	) 
	else
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

let string_of_input = 
  let coord_cmd label x y z e rest =
    let f label x = 
      BatOption.default "" **>
	BatOption.map ((^) (" " ^ label)) **>
	BatOption.map (Printf.sprintf "%.4f") x
    in
      label ^ f "X" x ^ f "Y" y ^ f "Z" z ^ f "E" e ^ " " ^ rest
  in
  function
  | G1 { x; y; z; e; rest } -> coord_cmd "G1" x y z e rest
  | M92 { x; y; z; e; rest } -> coord_cmd "M92" x y z e rest
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

let rec interpolate_pair threshold g1_0 g1_2 =
  if BatOption.default false (BatOption.map ((<) threshold) (distance2_opt (g1_0.x, g1_0.y) (g1_2.x, g1_2.y))) 
  then 
    let g1_1 = midway_g1 g1_0 g1_2 in
    let g1_2 = { g1_2 with rest = "\n" } in
      interpolate_pair threshold g1_0 g1_1 @ interpolate_pair threshold g1_1 g1_2
  else (
    [g1_2]
  )

let interpolate threshold data =
  let rec coords_of g1_0 current =
    match current with
      | None -> raise BatEnum.No_more_elements
      | Some ((Other _) as code) ->
	  ([code], g1_0)
      | Some ((G1 g1_2)) ->
	  (List.map (fun x -> G1 x) (interpolate_pair threshold g1_0 g1_2),
	   { x = coalesce2 g1_2.x g1_0.x;
	     y = coalesce2 g1_2.y g1_0.y;
	     e = coalesce2 g1_2.e g1_0.e;
 	     z = coalesce2 g1_2.z g1_0.z;
	     rest = "" })
      | Some ((M92 g) as code) ->
	  ([code], g)
  in
    BatEnum.concat (
      BatEnum.from_loop 
	{ x = None; y = None; z = None; e = None; rest = "" }
	(fun prev ->
	   let (list, prev') = coords_of prev (BatEnum.get data) in
	     (BatList.enum list, prev')
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

let map ~x_dim ~y_dim ~bump_height ~x_attach_factor ~y_attach_factor ~zx_delta ~zy_delta ~offset (x, y, z) =
  let x_bump = bump 2.0 x_dim x in
  let y_bump = bump 2.0 y_dim y in
    z 
    +. (bump_height *. (midway x_bump y_bump)) 
    *. (bump x_attach_factor (x_dim +. 2.0) (x +. 1.0))
    *. (bump y_attach_factor (y_dim +. 2.0) (y +. 1.0))
    +. interpolate1 (0.0, x_dim) (-. zx_delta /. 2.0, zx_delta /. 2.0) x
    +. interpolate1 (0.0, y_dim) (-. zy_delta /. 2.0, zy_delta /. 2.0) y
    +. offset

type env = {
  step_size : float;
  mapping   : input -> input;
}

let transform { step_size; mapping } =
  let data = parse_gcode () in
  let data = BatEnum.map mapping data in
  let data = interpolate step_size data in
  let data = BatEnum.map string_of_input data in
    BatEnum.iter print_string data

let main () =
  (* let input = BatStd.input_chars Pervasives.stdin in *)
  let bump_height = ref 0.0 in
  let zx_delta = ref 0.0 in
  let zy_delta = ref 0.0 in
  let x_dim = ref 160 in
  let y_dim = ref 199 in
  let offset = ref 0.0 in
  let step_size = ref 50.0 in
  let mode = ref transform in
    Arg.parse [("-b", Arg.Set_float bump_height, "Set bump height at the center");
	       ("-xd", Arg.Set_float zx_delta, "Set height difference from the beginning to end of X axis");
	       ("-yd", Arg.Set_float zy_delta, "Set height difference from the beginning to end of Y axis");
	       ("-ofs", Arg.Set_float offset, "Set offset");
	       ("-step", Arg.Set_float step_size, "Set traveled distance that is interpolated");
	       ("-x", Arg.Set_int x_dim, "Set area X size");
	       ("-y", Arg.Set_int y_dim, "Set area Y size");
	      ] (fun arg -> Printf.ksprintf failwith "Invalid argument: %s\n%!" arg) "G-code leveler";
    let bump_height = !bump_height in
    let x_dim = float !x_dim in
    let y_dim = float !y_dim in
    let zx_delta = !zx_delta in
    let zy_delta = !zy_delta in
    let offset = !offset in
    let mapping = (map_z (map ~offset ~x_dim ~y_dim ~bump_height ~zx_delta ~zy_delta ~x_attach_factor:2.0 ~y_attach_factor:20.0)) in
    let step_size = !step_size in
      !mode { step_size; mapping }

let _ = main ()
