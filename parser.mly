%token <string> ID
%token <int> NUM
%token IN VARS DOT
%token IFZ THEN ELSE WHILE DO
%token SKIP SCOLON
%token ASSIGN COMMA INITIALIZE
%token PLUS MINUS PROD CASE TUPLE
%token LPAR RPAR BLOCKBEG BLOCKEND LINDEXER RINDEXER
%token TYPE PTR NEW
%token TNAT TTUPLE
%token TSUM PIPE OF
%token FUNC RETURN 
%token EOF
 
%right DOT
%left PLUS
%left PROD
%left SCOLON
%left MINUS

%start <Syntax.program> prog

%%
prog:
  | global_types = gtype_dcl*;
    global_funcs = gfunc_dcl*; 
    VARS; LPAR; global_vars = gvar_inits; RPAR; IN; 
    BLOCKBEG; c = command; BLOCKEND; 
    EOF {Syntax.make_globals (global_types @ global_funcs @ global_vars) c}

gtype_dcl:
  | TYPE; x = ID; ASSIGN; t = typ; SCOLON {Syntax.GType(x, t)}

gfunc_dcl:
  | FUNC; f = ID; LPAR; args = args; RPAR; ASSIGN; 
    VARS; LPAR; declarations = var_inits; RPAR; IN; BLOCKBEG; c = command; BLOCKEND; 
    RETURN; e = expr; SCOLON; {Syntax.GFunc(f, args, declarations, c, e)}

args:
  | args = separated_list(COMMA, arg) {args}

arg:
  | t = typ; x = ID {(x, t)}

pipedtypes:
  | pipedtypes = separated_nonempty_list(PIPE, pipedtype) {pipedtypes}

pipedtype:
  | x = ID; OF; t = typ {(x, t)}

types:
  | types = separated_list(COMMA, typ) {types}
  
typ:
  | TNAT; {Syntax.TNat}
  | TTUPLE; LPAR; ts = types; RPAR {Syntax.TTuple ts}
  | TSUM; LPAR; ts = pipedtypes; RPAR; {Syntax.TSum ts}
  | PTR; LPAR; t = typ; RPAR; {Syntax.TPtr t}
  | x = ID {Syntax.TAbs x}

gvar_inits:
  | gvar_inits = separated_list(COMMA, gvar_init) {gvar_inits}

gvar_init:
  | x = ID; INITIALIZE; e = expr {Syntax.GVar(x,e)}
  | t = typ; x = ID; INITIALIZE; e = expr; {Syntax.GTypedVar(x, e, t)}
  | x = ID; INITIALIZE; NEW; e = expr {Syntax.GPointer(x,e)}
  | t = typ; x = ID; INITIALIZE; NEW; e = expr; {Syntax.GTypedPointer(x, e, t)}
  
command:
  | SKIP {Syntax.CSkip}
  | IFZ; e = expr; 
      THEN; BLOCKBEG; c1 = command; BLOCKEND; 
      ELSE; BLOCKBEG; c2 = command; BLOCKEND {Syntax.CIfThenElse(e, c1, c2)} 
  | WHILE; e = expr; 
      DO; BLOCKBEG; c = command; BLOCKEND {Syntax.CWhile(e, c)}
  | VARS; LPAR; declarations = var_inits; RPAR; IN; BLOCKBEG; c = command; BLOCKEND {Syntax.declare_vars declarations c}
  | x = ID; ASSIGN; f = ID; LPAR; es = exprs RPAR; {Syntax.CFuncCall(x, f, es)}
  | x = ID; ASSIGN; e = expr {Syntax.CAssign(x, e)}
  | c1 = command; SCOLON; c2 = command {Syntax.CCompose (c1, c2)}
  | PROD; x = ID; ASSIGN; e = expr {Syntax.CPointerAssign(x, e)}

exprs:
  | exprs = separated_list(COMMA, expr) {exprs}

expr:
  | TUPLE; LPAR; es = exprs; RPAR {Syntax.ETuple es}
  | PROD; e = expr {Syntax.EPoint e}
  | x = ID; DOT; e = expr; {Syntax.EInj(x, e)}
  | x = ID {Syntax.EVar x}
  | n = NUM {Syntax.ENat n}
  | x1 = expr; PLUS; x2 = expr {Syntax.EPlus(x1, x2)}
  | x1 = expr; PROD; x2 = expr {Syntax.EMult(x1, x2)}
  | MINUS; x = expr; {Syntax.EMinus x}
  | LPAR; e = expr; RPAR {e}
  | e1 = expr; LINDEXER; e2 = expr; RINDEXER {Syntax.ETupIndex(e1, e2)} 
  | CASE; e = expr; BLOCKBEG; cs = cases; BLOCKEND {Syntax.ECase(e, cs)}
  
var_inits:
  | vars = separated_list(COMMA, var_init) {vars}

var_init:
  | t = typ; v = ID; INITIALIZE; e = expr; {Syntax.LTypedVar(v,e, t)}
  | x = ID; INITIALIZE; e = expr {Syntax.LVar(x,e)}
  | x = ID; INITIALIZE; NEW; e = expr {Syntax.LPointer(x,e)}
  | t = typ; x = ID; INITIALIZE; NEW; e = expr; {Syntax.LTypedPointer(x, e, t)}
  
cases:
  | cs = separated_nonempty_list(PIPE, case) {cs}

case:
  | l = ID; DOT; x = ID; OF; e = expr {(l, x, e)}
  