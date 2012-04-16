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
  | (['a'-'z' 'A' - 'Z'] as reg) (['0'-'9' '.']* as value)
      { Entry (reg, 
	       if Pcre.pmatch ~pat:"\\." value
	       then Float (float_of_string value)
	       else Int (int_of_string value)) }
  | [' ' '\t'] *
      { token lexbuf }
  | ';' ([^ '\n']* as comment) { Comment comment }
  | '\n' { Eol }
  | eof { Eof }
  | _ { assert false }
