%token <int> INT
%token <float> REAL
%token <string> INPUT_IDENT PROP_IDENT 
%token EOF COLON COMMA LPAREN RPAREN
%token NOT
%token IMPLIES
%token DIVIDE MULTIPLY
%token PLUS MINUS
%token GE GT LE LT 
%token NOT_EQUAL EQUAL 
%token AND
%token OR 
%token CONSTANT INPUT OUTPUT INTERNAL TOGGLES LATER IS ALWAYS FOR NEVER PREVIOUSLY_HELD GLOBALLY IS_SUCCEEDED_BY AFTER BETWEEN AND2 EVERY BEFORE UNTIL ONCE BECOME_SATISFIED IF THEN AT_LEAST AT_MOST LESS_THAN TIME_UNITS AFTERWARDS HOLDS DOUBLE_QUOTE
%token BOOLEAN_TYPE INTEGER_TYPE REAL_TYPE TRUE FALSE 

%{ open Ast_types %} 

%type <Ast_types.prog> prog
%type <Ast_types.req_with_id> req_with_id
%type <Ast_types.req> req
%type <Ast_types.var_type> var_type
%type <Ast_types.const_value> const_value
%type <Ast_types.declaration> declaration
%type <Ast_types.exp> exp

%start prog

%%

prog :
| EOF ; { Prog (None, None) }
| declarations_ = list (declaration) ?; reqs_ = list(req_with_id) ?; EOF;  { Prog ( declarations_ , reqs_) }
;

var_type :
| BOOLEAN_TYPE ; { Bool } 
| INTEGER_TYPE ; { Int }
| REAL_TYPE ; { Real}

const_value:
| TRUE; { Const_bool (true) }
| FALSE; { Const_bool (false) }
| r_ = REAL; { Const_real (r_) }
| i_ = INT; { Const_int (i_) }
;

declaration :
| CONSTANT; id_ = INPUT_IDENT; IS; t_ = const_value; { Constant  (id_, t_) }
| INPUT;    id_ = INPUT_IDENT; IS; t_ = var_type;    { Input  (id_, t_) }
| OUTPUT;    id_ = INPUT_IDENT; IS; t_ = var_type;    { Output  (id_, t_) }
| INTERNAL;    id_ = INPUT_IDENT; IS; t_ = var_type;    { Internal  (id_, t_) }
;

exp :
| DOUBLE_QUOTE; e1_ = exp; DOUBLE_QUOTE; { e1_}
| LPAREN; e1_ = exp; RPAREN; { e1_ }
| e1_ = exp; PLUS e2_ =exp; { Plus (e1_, e2_ )}
| e1_ = exp; MINUS e2_ =exp; { Minus (e1_, e2_ )}
| e1_ = exp; DIVIDE e2_ =exp; { Divide (e1_, e2_ )}
| e1_ = exp; MULTIPLY e2_ =exp; { Multiply (e1_, e2_ )}
| e1_ = exp; OR ; e2_ = exp; { Or (e1_, e2_ )}
| e1_ = exp; AND ; e2_ = exp; { And (e1_, e2_ )}
| e1_ = exp; EQUAL ; e2_ = exp; { Eq (e1_, e2_ )}
| e1_ = exp; NOT_EQUAL ; e2_ = exp; { NotEq (e1_, e2_ )}
| e1_ = exp; GE ; e2_ = exp; { Geq (e1_, e2_ )}
| e1_ = exp; GT ; e2_ = exp; { Gt (e1_, e2_ )}
| e1_ = exp; LE ; e2_ = exp; { Leq (e1_, e2_ )}
| e1_ = exp; LT ; e2_ = exp; { Lt (e1_, e2_ )}
| e1_ = exp; IMPLIES; e2_ = exp; { Implies (e1_,e2_) }
| NOT; e1_ = exp; { Not(e1_) }
| id_ = INPUT_IDENT; { Var ( id_) }
| r_ = REAL; { Real_const (r_) }
| i_ = INT; { Int_const (i_) }
| TRUE ; { Bool_const(true) }
| FALSE; { Bool_const(false) }
;

hold:
| HOLDS; { Holds }
| HOLDS; AFTERWARDS;  {Holds_afterward}
| HOLDS; AFTERWARDS; FOR; AT_LEAST; e1_ = exp; TIME_UNITS;  {Holds_afterward_for_at_least (e1_)}
| HOLDS; FOR; AT_LEAST; e1_ = exp; TIME_UNITS; { Holds_for_at_least (e1_ ) }
| HOLDS; AFTER; AT_MOST; e1_ = exp; TIME_UNITS;  {Holds_after_at_most (e1_ )}
| HOLDS; FOR; LESS_THAN; e1_ = exp; TIME_UNITS; { Holds_for_less_than (e1_ ) }
| HOLDS; AT_LEAST; EVERY; e1_ = exp;  TIME_UNITS; {Holds_at_list_every(e1_)}
| HOLDS; AND2; IS_SUCCEEDED_BY; e1_ = exp; {Holds_end_succeded_by(e1_)}
| PREVIOUSLY_HELD; { Previously_held }
| TOGGLES; e1_ =exp; AT_MOST; e2_=exp; TIME_UNITS; LATER; { Toggles_at_most (e1_ , e2_)}

exp_hold :
| DOUBLE_QUOTE; e1_ = exp; DOUBLE_QUOTE; h_ = hold; { Prop (e1_, h_) }
| ONCE; e1_=exp; BECOME_SATISFIED; COMMA?; h_=hold; { Prop (e1_, h_) }
;

req :
| e1_ = exp_hold; { e1_ }
| GLOBALLY; COMMA; r_ =req; { Globally (r_) }
| NEVER; r_ = req; { Never ( r_ ) }
| ALWAYS; r_ = req; { Always (r_ ) }
| AFTER ; e1_ = exp; COMMA; r2_ = req; { After ( e1_, r2_) }
| AFTER; e1_=exp; UNTIL; e2_=exp; COMMA; r2_ = req; { After_until ( e1_, e2_, r2_) }
| BEFORE ; e1_ = exp; COMMA; r2_ = req; { Before ( e1_, r2_) }
| IF; r1_ = req; COMMA?; THEN; r2_ = req; { If (r1_, r2_ ) } 
| BETWEEN; e1_ =exp; AND2; e2_ =exp; COMMA; r_ =req; { Between (e1_, e2_, r_)}
;

req_with_id :
| id_ = PROP_IDENT; COLON; req_ = req; { Req (id_, req_) }
;

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }

