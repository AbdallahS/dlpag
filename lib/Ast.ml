open Printf

include Types.AST
open T

module Print =
struct
  let id x = x
  let cname = id

  let eoperator_aux = function
    | Add -> "+"
    | Mult -> "*"
    | Max -> "\\max"
    | Min -> "\\min"
    | Pow -> "**"
  let foperator_aux = function
    | Conj -> "and"
    | Disj -> "or"
  let poperator_aux = function
    | Seq -> "seq"
    | U -> "cup"
  let roperator = function
    | Eq -> "="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="
  let soperator_aux = function
    | Intersect -> "intersect"
    | Union -> "union"
  let eoperator o = "\\" ^ eoperator_aux o
  let foperator o = "\\" ^ foperator_aux o
  let poperator o = "\\" ^ poperator_aux o
  let soperator o = "\\" ^ soperator_aux o
  let bigeoperator o = "\\big" ^ eoperator_aux o
  let bigfoperator o = "\\big" ^ foperator_aux o
  let bigpoperator o = "\\big" ^ poperator_aux o
  let bigsoperator o = "\\big" ^ soperator_aux o

  let list_tuple f = Print.list' "(" ", " ")" f
  let rec pure_term : pure_term -> string = function
    | PVar v -> v
    | PFun (c, ts) -> sprintf "%s%s" c (list_tuple pure_term ts)
    | PInt i -> sprintf "%d" i

  let rec set = function
    | Set (t, []) -> sprintf "{ %s }" (Print.list' "" ", " "" element t)
    | Set (t, (_ :: _ as vs)) -> sprintf "{ %s | %s }" (Print.list' "" ", " "" element t) (vdecls vs)
    | CallS c -> callable c
    | ListS (o, s, ss) -> Print.list' "" (sprintf " %s " (soperator o)) "" set (s :: ss)
    | BigS (o, vs, e) -> sprintf "%s %s, %s" (bigsoperator o) (vdecls vs) (set e)
    | Setminus (s, ss) -> Print.list' "" " \\setminus " "" set (s :: ss)
  and vdecls ds = Print.list' "" ", " "" vdecl ds
  and vdecl = function
    | FromSet (term, s) -> sprintf "%s \\in %s" (pure_term term) (set s)
    | Constraint c -> constraints c
  and constraints = function
    | In (t, s) -> sprintf "%s \\in %s" (term t) (set s)
    | Notin (t, s) -> sprintf "%s \\notin %s" (term t) (set s)
    | Relation (r, t1, t2) -> sprintf "%s %s %s" (term t1) (roperator r) (term t2)
  and element = function
    | Tuple t -> term t
    | Range (e1, e2) -> sprintf "%s..%s" (expr e1) (expr e2)
  and term : term -> string = function
    | Fun (c, ts) -> sprintf "%s%s" c (list_tuple term ts)
    | Exp e -> expr e
    | Var n -> n
  and expr = function
    | ListE (a, e, es) -> Print.list' "" (sprintf " %s " (eoperator a)) "" inner_expr (e :: es)
    | BigE (a, vs, e) -> sprintf "%s %s, %s" (bigeoperator a) (vdecls vs) (expr e)
    | Subtract (v, vs) -> Print.list' "" " - " "" inner_expr (v :: vs)
    | VarE _ | Int _ as e -> inner_expr e
  and inner_expr = function
    | VarE n -> n
    | Int i -> Print.int i
    | ListE _ | BigE _ | Subtract _ as e -> sprintf "(%s)" (expr e)
  and callable = function
    | Call (n, []) -> cname n
    | Call (n, (_ :: _ as ts)) -> sprintf "%s(%s)" n (Print.list' "" ", " "" term ts)
    | VarC n -> n

  let rec formula = function
    | Top | CallF _ | Neg _ | Diamond _ as f -> inner_formula f
    | ListF (a, f, fs) -> Print.list' "" (sprintf " %s " (foperator a)) "" inner_formula (f :: fs)
    | BigF (a, vs, f) -> sprintf "%s %s, %s" (bigfoperator a) (vdecls vs) (formula f)
  and inner_formula = function
    | Top -> "\\top"
    | CallF a -> callable a
    | Neg f -> sprintf "\\neg %s" (inner_formula f)
    | Diamond (p, f) -> sprintf "<%s>%s" (program p) (inner_formula f)
    | ListF _ | BigF _  as f -> sprintf "(%s)" (formula f)
  and program = function
    | ListP (a, p, ps) -> Print.list' "" (sprintf " %s " (poperator a)) "" inner_program (p :: ps)
    | BigP (a, vs, p) -> sprintf "%s %s, %s" (bigpoperator a) (vdecls vs) (program p)
    | CallP _ | Assign _ | Test _ | Converse _ | Kleene _ as p -> inner_program p
  and inner_program = function
    | CallP a -> callable a
    | Assign (a, f) -> sprintf "%s <- %s" (callable a) (formula f)
    | Test f -> sprintf "%s?" (formula f)
    | Converse p -> sprintf "%s^" (program p)
    | Kleene p -> sprintf "%s\\star" (program p)
    | ListP _ | BigP _ as p -> sprintf "(%s)" (program p)

  let forall_decls vs = match vs with
    | [] -> ""
    | _ :: _ -> sprintf "\\forall %s, " (vdecls vs)
  let decl pr (vs, c, a) = sprintf "%s%s := %s." (forall_decls vs) (callable c) (pr a)
  let main_decl m = sprintf "%s." (callable m)

  let file (gs, fs, ps, m) = sprintf "grounding:\n%s\nformula:\n%s\nprogram:\n%smain:\n%s" (Print.unlines (decl set) gs) (Print.unlines (decl formula) fs) (Print.unlines (decl program) ps) (main_decl m)
end
