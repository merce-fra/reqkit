(* Copyright 2025 Mitsubishi Electric R&D Centre Europe
 * Author: François Cellier
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
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
let start_var_decl = ['a'-'z' 'A'-'Z']
let input_ident =  ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']*
let smt_ident = ['''] input_ident  [''']

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule singleline_comment = parse
  | '\n'   { Lexing.new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

and multiline_comment = parse
  | '\n'   { Lexing.new_line lexbuf ; multiline_comment lexbuf}
  | eof    { () }
  | "\"\"\""   { () }
  | _      { multiline_comment lexbuf }

and ignore_line = parse
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
| "#"
    { singleline_comment lexbuf; token lexbuf } 
| "\"\"\""
    { multiline_comment lexbuf; token lexbuf } 
| "from"
| "ALPHA"
| "BETA"
| "REQ_MODIFY"
| "MAX_PTRACE"
    { ignore_line lexbuf; token lexbuf } 
| "Bool"
    { BOOLEAN_TYPE }
| "True"
    { TRUE }
| "False"
    { FALSE }
| "REQ_SET" 
    { REQ_SET}  
| "COND_INIT"
    { COND_INIT }
| "Not"
    { NOT }
| '='
    { EQUAL }   
| "=="
    {EQUAL_TO}
| ','
    { COMMA }    
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '['
    { LSQUARE }
| ']'
    { RSQUARE }
| "Or"
    { OR }
| "And" 
    { AND }
| input_ident as s
    { INPUT_IDENT (s) }
| smt_ident as s 
    { SMT_IDENT (s)}
| integer as i 
    { INT(int_of_string i) }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
| eof
    { raise (Error (Printf.sprintf "At offset %d: unexpected end of input.\n" (Lexing.lexeme_start lexbuf))) }

