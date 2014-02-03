open BatPervasives

module Lnoexn = BatList.Exceptionless

let ( **> ) a b = a b

type goto =
    { x : float option;
      y : float option;
      z : float option;
      e : float option;
      rest : string; }

type rest = string

type input = 
  | G0 of goto			     (* go to point *)
  | G1 of goto			     (* go to point *)
  | G90abs of rest		     (* switch to absolute movement *)
  | G91rel of rest		     (* switch to relative movement *)
  | G92 of goto			     (* set values *)
  | Other of string

let string_of_gfloat f =
  let str = Printf.sprintf "%.5f" f in
  let last_non_zero =
    let rec find_last_nonzero ofs = 
      if ofs < 0
      then None
      else if str.[ofs] <> '0'
      then Some ofs
      else find_last_nonzero (ofs - 1)
    in
    find_last_nonzero (String.length str - 1)
  in
  ( match last_non_zero with
  | None -> str
  | Some ofs when str.[ofs] = '.' -> String.sub str 0 (ofs + 2)
  | Some ofs -> String.sub str 0 (ofs + 1)
  )

let string_of_token = function
  | Lexer.Eof -> ""
  | Lexer.Entry (register, Lexer.Int value) -> Printf.sprintf "%c%d" register value
  | Lexer.Entry (register, Lexer.Float value) -> Printf.sprintf "%c%s" register (string_of_gfloat value)
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
    let g0 = List.mem (Lexer.Entry ('G', Lexer.Int 0)) accu in
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
	| _ when g0 ->
	    let (x, y, z, e) = new_at in
	      update_positions ();
	      (G0 { x; y; z; e; rest = Lazy.force rest })
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
      | `Absolute, Some (G0 { x; y; z; e; rest } | G1 { x; y; z; e; rest }) -> (x, y, z, e)
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
		| Some x', Some x when string_of_gfloat x = string_of_gfloat x' -> ""
		| _, Some x -> Printf.sprintf " %s%s" label (string_of_gfloat x) )
	| `Relative ->
	    match x with
	      | Some x when string_of_gfloat x <> string_of_gfloat 0.0 -> Printf.sprintf " %s%s" label (string_of_gfloat x)
	      | None | Some _ -> ""
    in
      label ^ f "X" x x' ^ f "Y" y y' ^ f "Z" z z' ^ f "E" e e' ^ " " ^ rest
  in
  function
  | G0 { x; y; z; e; rest } -> coord_cmd "G0" x y z e rest
  | G1 { x; y; z; e; rest } -> coord_cmd "G1" x y z e rest
  | G92 { x; y; z; e; rest } -> coord_cmd "G92" x y z e rest
  | G90abs rest -> "G90 " ^ rest
  | G91rel rest -> "G91 " ^ rest
  | Other str -> str
