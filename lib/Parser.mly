%token <string> VNAME
%token <string> CNAME
%token <int> INT
%token IN NOTIN
%token FORALL BIGCONJ BIGDISJ BIGSEQ BIGNONDET
%token BOT TOP NEG CONJ DISJ LANGLE RANGLE
%token ASSIGN TEST SEQ NONDET CONVERSE STAR
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DEFINE COLON COMMA DOT RANGE MID WHERE
%token PLUS MULT POW MINUS (*eop*)
%token EQ NEQ LEQ GEQ
%token BIGPLUS BIGMULT MAX MIN (*beop*)
%token UNION INTERSECT SETMINUS (* sop *)
%token BIGUNION BIGINTERSECT (* bsop *)
%token GROUND FORMULA PROGRAM MAIN
%token EOF

%{
open Ast.T
(*module Ast = Ast.T*)
(*let nest_quants l phi =
  let rec aux accu = function
    | [] -> accu
    | a :: r -> aux (Ast.QuantF (a, accu)) r in
  aux phi (List.rev l)*)
%}
(*%type <(Ast.keyword, Ast.free) Ast.atomic> keyword_atomic*)
%type <Ast.T.file> file
%start file
%%

%inline soption(X):
|        { "" } /* nothing */
| x = X  { x }

separated_many_slist(Sep, Sub):
| a = Sub s = Sep r = separated_nonempty_list(Sep, Sub) { (s, a, r) }

tuple_list(X):
| LPAREN l = separated_list(COMMA, X) RPAREN { l }
otuple_list(X):
| { [] }
| l = tuple_list(X) { l }
ttuple_list(X):
| LANGLE l = separated_list(COMMA, X) RANGLE { l }

doperator: | FORALL { ()  }
eoperator: | PLUS { Add } | MULT { Mult } | POW { Pow }
foperator: | CONJ { Conj } | DISJ { Disj }
poperator: | SEQ { Seq } | NONDET { U }
roperator: | EQ { Eq } | NEQ { Neq } | LANGLE { Lt } | RANGLE { Gt } | LEQ { Leq } | GEQ { Geq }
soperator: | UNION { Union } | INTERSECT { Intersect }
beoperator: | BIGPLUS { Add } | BIGMULT { Mult } | MAX { Max } | MIN { Min }
bfoperator: | BIGCONJ { Conj } | BIGDISJ { Disj }
bpoperator: | BIGSEQ { Seq } | BIGNONDET { U }
bsoperator: | BIGUNION { Union } | BIGINTERSECT { Intersect }

pure_term:
| vs = ttuple_list(inner_pure_term) { PFun ("", vs) }
| t = inner_pure_term { t }
inner_pure_term:
| name = CNAME vs = otuple_list(inner_pure_term) { PFun (name, vs) }
| v = VNAME { PVar v }

term:
| ts = ttuple_list(inner_term) { Fun ("", ts)  }
| t = inner_term { t }
inner_term:
| name = CNAME ts = otuple_list(inner_term) { Fun (name, ts)  }
| v = VNAME { Var v }
| e = cexpr { Exp e }

set:
| s = inner_set { s }
outer_set:
| LBRACE cs = separated_list(COMMA, element) vs = loption(MID vs = vdecls { vs }) RBRACE { Set (cs, vs) }
| c = callable { CallS c }
| LPAREN s = inner_set RPAREN { s }
inner_set:
| l = separated_many_slist(soperator, outer_set) { ListS l }
| l = separated_many_slist(SETMINUS, outer_set) { let (), hd, tl = l in Setminus (hd, tl) }
| o = bsoperator vs = vdecls COLON f = inner_set { BigS (o, vs, f) }
| s = outer_set { s }

constraints:
| LPAREN c = constraints RPAREN { c }
| t = term IN s = set { In (t, s)}
| t = term NOTIN s = set { Notin (t, s)}
| t1 = term r = roperator t2 = term { Relation (r, t1, t2) }

vdecl:
| vs = pure_term IN s = set { FromSet (vs, s) }
| WHERE c = constraints { Constraint c }

vdecls:
| l = separated_nonempty_list(COMMA, vdecl) { l }

element:
| t = term { Tuple t  }
| e1 = expr RANGE e2 = expr { Range (e1, e2) }

cexpr:
| l = separated_many_slist(eoperator, outer_expr) { ListE l }
| l = separated_many_slist(MINUS, outer_expr) { let ((), h, r) = l in Subtract (h, r) }
| o = beoperator vs = vdecls COLON f = inner_expr { BigE (o, vs, f) }
| i = INT { Int i }
| MINUS i = INT { Int (-i) }
expr:
| e = inner_expr { e }
outer_expr:
| n = VNAME { VarE n }
| i = INT { Int i }
| MINUS i = INT { Int (-i) }
| LPAREN e = inner_expr RPAREN { e }
inner_expr:
| l = separated_many_slist(eoperator, outer_expr) { ListE l }
| l = separated_many_slist(MINUS, outer_expr) { let ((), h, r) = l in Subtract (h, r) }
| o = beoperator vs = vdecls COLON f = inner_expr { BigE (o, vs, f) }
| e = outer_expr { e }

callable:
| n = CNAME ts = otuple_list(term) { Call (n, ts) }
| n = VNAME { VarC n }

decl(A):
| vs = loption(doperator vs = vdecls COLON { vs }) c = callable DEFINE a = A DOT { (vs, c, a) }

formula:
| f = inner_formula { f }
outer_formula:
| c = callable { CallF c }
| TOP { Const Top}
| BOT { Const Bot }
| NEG f = outer_formula { Neg f }
| LANGLE   p = program RANGLE   f = outer_formula { Modal (Diamond, p, f) }
| LBRACKET p = program RBRACKET f = outer_formula { Modal (Box, p, f) }
| LPAREN f = inner_formula RPAREN { f }
inner_formula:
| l = separated_many_slist(foperator, outer_formula) { ListF l }
| o = bfoperator vs = vdecls COLON f = inner_formula { BigF (o, vs, f) }
| f = outer_formula { f }

program:
| p = inner_program { p }
outer_program:
| c = callable { CallP c }
| c = callable ASSIGN f = formula { Assign (c, f) }
| TEST f = inner_formula TEST { Test f }
| p = outer_program CONVERSE { Converse p }
| p = outer_program STAR { Kleene p }
| LPAREN p = inner_program RPAREN { p }
inner_program:
| l = separated_many_slist(poperator, outer_program) { ListP l }
| o = bpoperator vs = vdecls COLON p = inner_program { BigP (o, vs, p) }
| p = outer_program { p }

main_decl:
c = callable DOT { c }

file: GROUND COLON gs = list(decl(set)) FORMULA COLON fs = list(decl(formula)) PROGRAM COLON ps = list(decl(program)) MAIN COLON m = main_decl EOF { (gs, fs, ps, m) }
