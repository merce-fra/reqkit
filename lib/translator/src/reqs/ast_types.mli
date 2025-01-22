(* Copyright 2025 Mitsubishi Electric R&D Centre Europe
 * Author: François Cellier
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

type var_type =
  | Bool (* "Input" ... "IS bool" *)
  | Int (* "Input" ... "IS int" *)
  | Real (* "Input" ... "IS real" *)

type const_value =
  | Const_bool of bool (* "CONST" ... "IS" ...*)
  | Const_int  of int (* "CONST" ... "IS" ...*)
  | Const_real of float (* "CONST" ... "IS" ...*)

type declaration =
  | Constant of string * const_value (* "CONST" ... "IS" ...*)
  | Input of string * var_type (* "Input" ... "IS" ...*)
  | Output of string * var_type (* "Output" ... "IS" ...*)
  | Internal of string * var_type (* "Internal" ... "IS" ...*)

(* Note: all [exp] are surrounded by double quotes *)
type exp = | Var of string (* xNNNNN *)
         | Bool_const of bool (* true *)
         | Int_const of int (* 10 *)
         | Real_const of float (*10.0*)
         | Not of exp (* "!" exp *)
         | And of exp * exp (* exp "&&" exp *)
         | Or of exp * exp (* exp "||" exp *)         
         | Eq of exp * exp (* exp "==" exp *)
         | NotEq of exp * exp (* exp "!=" exp *)
         | Geq of exp * exp (* exp ">=" exp *)
         | Leq of exp * exp (* exp "<=" exp *)
         | Gt of exp * exp (* exp ">" exp *)
         | Lt of exp * exp (* exp "<" exp *)
         | Implies of exp * exp (* exp ==> exp *)
         | Plus of exp * exp (* exp + exp *)
         | Minus of exp * exp (* exp - exp *)
         | Divide of exp * exp (* exp / exp *)
         | Multiply of exp * exp (* exp * exp *)

type hold=
  | Empty (* no info *)
  | Holds (* holds or holds as well *)
  | Holds_afterward (* holds afterwards *)
  | Previously_held (* previously held *)
  | Holds_for_at_least of exp   (*holds for at least [exp] time units*) 
  | Holds_after_at_most of exp   (*holds after at most [exp] time units*) 
  | Holds_afterward_for_at_least of exp (*holds afterwards for at least [exp] time units*) 
  | Holds_for_less_than of exp  (*holds for less [exp] time units*) 
  | Holds_at_least_every of exp  (*holds at least every [exp] time units*) 
  | Holds_and_succeeded_by of exp  (*holds and is succeeded by [exp]*) 
  | Holds_indefinitely (*used for internal mechanism*)
  | At_most of exp  (*at most [exp] time units*) 
         
type req =
  | Prop of exp * hold (* [exp] "holds" or [exp] "holds as well" *)
  | Globally of req (* "Globally" "," [req] *)
  | After of exp * req (* "After" [exp] "," [req] *)
  | After_until of exp * exp * req (* "After" [exp] "until" [exp] "," [req] *)
  | Before of exp *req (* "Before" [req] "," *)
  | Always of req (* "it is always the case that" [req] *)
  | Never of req (* "it is never the case that" [req] *)
  | If of req * req (* "if" [req], "then" [req] *)
  | After_at_most of req * exp (* [req] "after at most" [exp] "time units" *)
  | Between of exp *exp*req (* Between [exp] and [exp], [req]*)
  | Toggles of exp * exp * hold (* [exp] toggles [exp]  [hold] *)


type req_with_id = | Req of string * req

type prog = | Prog of declaration list option * req_with_id list option

