{
  type register = char

  type value = 
    | Int of int
    | Float of float

  type token =
    | Entry of (register * value)
    | Eol
    | Comment of string
    | Eof
}

rule token = parse
  | (['a'-'z' 'A' - 'Z'] as reg) (['0'-'9' '.' '-']* as value)
      { Entry (reg, 
	       if Pcre.pmatch ~pat:"\\." value
	       then Float (float_of_string value)
	       else Int (int_of_string value)) }
  | ('(' [^ ')' '\n']* ')') as comment { Comment comment }
  | ([' ' '\t'] * ';' [^ '\n']*) as comment { Comment comment }
  | [' ' '\t' '\r'] *
      { token lexbuf }
  | '\n' { Eol }
  | eof { Eof }
  | _ { 
    let open Lexing in
    Printf.eprintf "Failed to parse at line %d, column %d\n" lexbuf.lex_start_p.pos_lnum lexbuf.lex_start_p.pos_cnum;
    assert false 
  }
