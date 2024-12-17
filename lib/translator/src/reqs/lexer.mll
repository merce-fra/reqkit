{
  open Parser

  exception Error of string

}

(* some usefull regexp *)
let digit             = ['0'-'9']
let positive_integer  = digit+
let integer           = ('-'?) positive_integer
let real              = integer ('.') positive_integer
let any_char = ['0'-'9' 'a'-'z' 'A'-'Z' '"' '!' '=' ' ' '_' '&' ',' ':']
(* input idents are like x0001 *)
let input_ident           = (['a'-'z' 'A'-'Z' '_']) (['a'-'z' 'A'-'Z' '_' '0'-'9'])*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule singleline_comment = parse
  | '\n'   { Lexing.new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)

and line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

and token = parse
| whitespace 
    { token lexbuf }
| newline
    { Lexing.new_line lexbuf;  token lexbuf }
| "//"
    { singleline_comment lexbuf; token lexbuf } 
| ("IS" | "is")
    { IS }
| "bool"
    { BOOLEAN_TYPE }
| "int"
    { INTEGER_TYPE }
| "real"
    { REAL_TYPE }
| ("true" | "TRUE")
    { TRUE }
| ("false" | "FALSE")
    { FALSE }
| "==>"
    { IMPLIES }
| "CONST"
    { CONSTANT }
| "Input"
    { INPUT }
| "Output"
    { OUTPUT }
| ("Internal" | "internal")
    { INTERNAL }    
| ':'
    { COLON }
| integer as i 
    { INT (int_of_string i)}
| real as r
    { REAL (float_of_string r)}    
| "it is always the case that"
    { ALWAYS }
| "it is never the case that"
    { NEVER }
| "Globally"
    { GLOBALLY }
| ("After" | "after")
    { AFTER }
| ("Before" | "before")
    { BEFORE }
| "until"
    { UNTIL }
| ("holds" | "holds as well" | "it holds" | "holds."  | "holds as well.")
    { HOLDS }
| "toggles"
    { TOGGLES }
| "if"
    { IF }  
| "then"
    { THEN } 
| "for"
    { FOR }
| ("previously held" | "previously held.")
    { PREVIOUSLY_HELD }
| "once"
    { ONCE }
| "becomes satisfied"
    { BECOME_SATISFIED }
| ("Between" | "between")
    { BETWEEN }
| "every"
    { EVERY }
| "less than"
    { LESS_THAN }
| "at most"
    { AT_MOST } 
| "at least"
    { AT_LEAST }
| ("time units" | "time units." | "time unit." )
    { TIME_UNITS }    
| "later"
    { LATER }
| "afterwards"
    {AFTERWARDS}
| "and"
    { AND2 }
| "is succeeded by"
    { IS_SUCCEEDED_BY }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "!="
    { NOT_EQUAL }
| '!'
    { NOT }
| '"'
    { DOUBLE_QUOTE }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '/'
    { DIVIDE }
| '*'
    { MULTIPLY }    
| ','
    { COMMA }    
| ("&&" | "∧")
    { AND }
| "||"
    { OR }
| ">=" 
    { GE }
| ">"
    { GT }
| "<=" 
    { LE }
| "<"
    { LT }
| "=="
    { EQUAL }
| input_ident as s
    { INPUT_IDENT (s) }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
| eof
    { raise (Error (Printf.sprintf "At offset %d: unexpected end of input.\n" (Lexing.lexeme_start lexbuf))) }

