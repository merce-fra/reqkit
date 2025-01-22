(* Copyright 2025 Mitsubishi Electric R&D Centre Europe
 * Author: François Cellier
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
 open Ast_types

type t = {
    vars: (string, declaration) Hashtbl.t;
      (* "CONST" [string] "IS" [var_or_const_type]
         or "Input" [string] "IS" [var_or_const_type] *)
    reqs: (string, req) Hashtbl.t (* [string] ":" [req] *)
  }

exception ParseException of string

(** [of_file filename] gets the content of [filename] as two hashmaps as [t]: one for declaration, other one for requirements *)
val of_file: string -> t   

(** [extract_bool_variables vars] extracts the boolean variables of the hashtable of declared variables [vars]*)                        
val extract_bool_variables : (string, declaration) Hashtbl.t -> string list 

(** [ast_to_parse_t ast] converts an [ast] of type Ast_types.Prog to a Parse_t.t *)
val ast_to_parse_t: (Ast_types.prog, string) result -> t 

(** [ast_from_string s] get the ast from a string [s]*)
val ast_from_string : string -> (Ast_types.prog, string) result

(** [print fmt r] prints the variables and the requirements in [r] in the formatter [fmt]*)  
val print : Format.formatter -> t -> unit  

(** [pretty_print fmt r] pretty prints the variables and the requirements in [r] in the formatter [fmt]*)  
val pretty_print : Format.formatter -> t -> unit  

(** [print_exp_as_string e] prints an expression [e] as a string *)
val print_exp_as_string : Ast_types.exp -> string 

(** [print_hold_as_string h] prints a hold [h] as a string *)
val print_hold_as_string : Ast_types.hold -> string 

(** [print_req_as_string h] prints a requirement [r] as a string *)
val print_req_as_string : Ast_types.req -> string 
 
(** [print_declaration_as_string d] prints a declaration [d] as a string *)
val print_declaration_as_string : Ast_types.declaration -> string 

