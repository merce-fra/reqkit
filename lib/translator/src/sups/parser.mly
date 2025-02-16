(* Copyright 2025 Mitsubishi Electric R&D Centre Europe
 * Author: François Cellier
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

%token <string> INPUT_IDENT SMT_IDENT  
%token <int> INT
%token EOF COMMA LPAREN RPAREN BOOLEAN_TYPE TRUE FALSE REQ_SET LSQUARE RSQUARE EQUAL_TO COND_INIT
%token EQUAL 
%token NOT
%token AND
%token OR 



%{ open Sup_types %} 

%type <Sup_types.prog> prog
%type <Sup_types.sup_req> sup_req
%type <Sup_types.event> event 
%type <Sup_types.declaration> declaration
%type <Sup_types.time> time
%start prog

%%

prog :
| declarations_ = declaration_list ; reqs_ = sup_req_list_decl ; inits_ = var_init_list; EOF;  { let  open Sup_types in {decls=declarations_ ; reqs=reqs_; inits=inits_ } }
;

declaration :
| id_ = INPUT_IDENT; EQUAL; BOOLEAN_TYPE; LPAREN;  id_smt_ = SMT_IDENT; RPAREN; { Decl  (id_, Boolean, id_smt_) }
;

declaration_list:
| EOF; {[]}
| l_=list (declaration); {l_}

time:
| i_= INT; {Time(i_)}

boolp:
| TRUE ; {true}
| FALSE ; {false}



event:
| b_= boolp;  {Constant(b_)}
| id_=INPUT_IDENT; {Var(id_)}
| AND; LPAREN; l_= separated_list(COMMA,event) ; RPAREN; { match l_ with 
                                                            | [] 
                                                            | _::[] -> raise (Syntax_error("And shall have at least two events"))
                                                            | hd::tail ->  List.fold_left (fun acc e -> And(e ,acc)) hd tail }
| OR; LPAREN; l_= separated_list(COMMA,event); RPAREN; {match l_ with 
                                                            | [] 
                                                            | _::[] -> raise (Syntax_error("Or shall have at least two events"))
                                                            | hd::tail ->  List.fold_left (fun acc e -> Or(e ,acc)) hd tail}
| NOT; LPAREN; e_= event; RPAREN; {Not(e_)}


sup_req :
| LSQUARE; 
  tse_ = event; COMMA;
  tc_  = event; COMMA;
  tee_ = event; COMMA;
  tmin_= time; COMMA;
  tmax_= time; COMMA;
  lmin_= time; COMMA;
  lmax_= time; COMMA;
  ase_ = event; COMMA;
  ac_  = event; COMMA;
  aee_ = event; COMMA;
  amin_= time; COMMA;
  amax_= time; 
  RSQUARE;
  { 
  let t_ : Sup_types.trigger = { tse= tse_; tc = tc_;tee =tee_;  tmin =tmin_;  tmax =tmax_ }  in
  let d_ : Sup_types.delay = {lmin=lmin_; lmax=lmax_} in
  let a_ : Sup_types.action = {ase= ase_; ac = ac_; aee =aee_;  amin =amin_;  amax =amax_} in
  {t=t_; d=d_; a=a_; vacuity=true}
  }

;

var_init:
| id_=INPUT_IDENT; EQUAL_TO; b_=boolp; {VarInit(id_,b_)}

sup_req_list_decl :
| REQ_SET; EQUAL; LSQUARE; l_ = separated_list(COMMA,sup_req) ; RSQUARE; {l_}
| EOF; {[]}

var_init_list:
| COND_INIT; EQUAL;  LSQUARE; l_ = separated_list(COMMA,var_init); RSQUARE; EOF; {l_}

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }

