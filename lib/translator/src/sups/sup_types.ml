(* Copyright 2025 Mitsubishi Electric R&D Centre Europe
 * Author: François Cellier
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
 type var_type = | Boolean 

exception Syntax_error of string

type declaration  = | Decl of string  * var_type * string

type event=  | Var of string (* xNNNNN *)
                | Not of event (* "!" exp *)
                | And of event * event (* exp "&&" exp *)
                | Or of event * event (* exp "||" exp *)         
                | Constant of bool
                | IntConstant of int*int
                | RealConstant of float*int
                | Plus of event * event (* exp + exp*)
                | Minus of event * event (* exp - exp*)
                | Multiply of event * event (* exp * exp*)
                | Divide of event * event (* exp / exp*)
                | Eq of event * event (* exp == exp*)
                | Geq of event * event (* exp >= exp*)
                | Leq of event * event (* exp <= exp*)
                | Gt of event * event (* exp > exp*)
                | Lt of event * event (* exp < exp*)
                | NotEq of event * event (* exp != exp*)

type time = | Time of int (*used for tmin, tmax, lmin, lmax, amin and amax*)            
            | GreaterThan of int (*used for real clock encoding*)
            | LesserThan of int (*used for real clock encoding*)

type trigger = { tse: event ; tc : event ; tee : event;  tmin : time;  tmax : time } (* trigger of a SUP *)

type action = { ase: event ; ac : event ; aee : event;  amin : time;  amax : time }  (* action of a SUP *)

type delay =  { lmin : time; lmax : time }                                           (* delay between end of the trigger and beginning of the action*)

type sup_req = {t : trigger; d : delay;  a : action;  vacuity : bool}                (* SUP requirement *)

type sup_req_list = sup_req list 

type var_init = | VarInit of string * bool  (* Var = True/False*)             

type cond_init = var_init list(* initializations in COND_INIT *) 

type prog = { decls: declaration list;  reqs: sup_req list; inits: cond_init}                                