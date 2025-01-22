(* Copyright 2025 Mitsubishi Electric R&D Centre Europe
 * Author: François Cellier
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
 open Reqs
open Sups

(** [sup_event_of_exp ast] converts a requirements [ast] to SUP event*)
let rec sup_event_of_exp ast = 
  match ast with
  | Ast_types.Not( e ) -> Sup_types.Not (sup_event_of_exp e)
  | Ast_types.And (e1, e2) -> Sup_types.And (sup_event_of_exp e1, sup_event_of_exp e2)
  | Ast_types.Or (e1, e2) -> Sup_types.Or (sup_event_of_exp e1, sup_event_of_exp e2)
  | Ast_types.Bool_const(b)-> Sup_types.Constant(b)
  | Ast_types.Var(s) -> Sup_types.Var(s)
  | _ -> raise (Invalid_argument ("(17) This node is not supported in SUP conversion " ^ (Reqs.Parse.print_exp_as_string ast)))

(*
(** [vmt_event_of_exp ast] converts an requirement expression [ast] into a vmt lib event*)
let rec vmt_event_of_exp clock_mult ast = 
  let vmt_event_of_exp_ = vmt_event_of_exp clock_mult in
  match ast with
  | Ast_types.Not( e ) -> Sup_types.Not (vmt_event_of_exp_ e )
  | Ast_types.And (e1, e2) -> Sup_types.And (vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)
  | Ast_types.Or (e1, e2) -> Sup_types.Or (vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)
  | Ast_types.Bool_const(b)-> Sup_types.Constant(b)
  | Ast_types.Int_const(i)-> Sup_types.IntConstant(i, clock_mult)
  | Ast_types.Real_const(f)-> Sup_types.RealConstant(f, clock_mult)
  | Ast_types.Var(s) -> Sup_types.Var(s)
  | Ast_types.Plus(e1,e2) -> Sup_types.Plus(vmt_event_of_exp_ e1,vmt_event_of_exp_ e2)
  | Ast_types.Minus(e1,e2) -> Sup_types.Minus(vmt_event_of_exp_ e1,vmt_event_of_exp_ e2)
  | Ast_types.Multiply(e1,e2) -> Sup_types.Multiply(vmt_event_of_exp_ e1,vmt_event_of_exp_ e2)
  | Ast_types.Divide(e1,e2) -> Sup_types.Divide(vmt_event_of_exp_ e1,vmt_event_of_exp_ e2)
  | Ast_types.Eq(e1,e2) -> Sup_types.Eq(vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)
  | Ast_types.NotEq(e1,e2) -> Sup_types.NotEq(vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)
  | Ast_types.Geq(e1,e2) -> Sup_types.Geq(vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)
  | Ast_types.Gt(e1,e2) -> Sup_types.Gt(vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)
  | Ast_types.Leq(e1,e2) -> Sup_types.Leq(vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)
  | Ast_types.Lt(e1,e2) -> Sup_types.Lt(vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)
  (*| Ast_types.Implies(e1,e2) -> Sup_types.Implies(vmt_event_of_exp_ e1, vmt_event_of_exp_ e2)*)
  | _ -> raise (Invalid_argument ("This node is not supported in VMT conversion " ^ (Parse.print_exp_as_string ast)))
*)

(** [equal ast1 ast2] checks if [ast1] and [ast2] are equals*)
let rec equal ast1 ast2 =
  match (ast1,ast2) with
  | (Ast_types.Not( e1 ), Ast_types.Not( e2 ))  -> equal e1 e2
  | (Ast_types.And (e1, e2),Ast_types.And (e3, e4)) 
  | (Ast_types.Or (e1, e2),Ast_types.Or (e3, e4)) 
  | (Ast_types.Plus(e1,e2),Ast_types.Plus(e3,e4)) 
  | (Ast_types.Minus(e1,e2),Ast_types.Minus(e3,e4)) 
  | (Ast_types.Multiply(e1,e2), Ast_types.Multiply(e3,e4) )
  | (Ast_types.Divide(e1,e2), Ast_types.Divide(e3,e4) ) 
  | (Ast_types.Eq(e1,e2), Ast_types.Eq(e3,e4)) 
  | (Ast_types.NotEq(e1,e2),Ast_types.NotEq(e3,e4)) 
  | (Ast_types.Geq(e1,e2),Ast_types.Geq(e3,e4)) 
  | (Ast_types.Gt(e1,e2),Ast_types.Gt(e3,e4)) 
  | (Ast_types.Leq(e1,e2),Ast_types.Leq(e3,e4)) 
  | (Ast_types.Lt(e1,e2),Ast_types.Lt(e3,e4)) -> (equal e1 e3) && (equal e2 e4)
  | (Ast_types.Bool_const(b),Ast_types.Bool_const(b2))-> b = b2
  | (Ast_types.Int_const(i),Ast_types.Int_const(i2))-> i = i2
  | (Ast_types.Real_const(f),Ast_types.Real_const(f2))-> f = f2
  | (Ast_types.Var(s), Ast_types.Var(s2)) -> String.equal s s2
  | _ -> false
