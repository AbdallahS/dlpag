module AST = struct module T = struct
type cname = string
type vname = string

type coperator = Top | Bot
type eoperator = Add | Mult | Max | Min | Pow
type foperator = Conj | Disj
type moperator = Box | Diamond
type poperator = Seq | U
type roperator = Eq | Neq | Lt | Gt | Leq | Geq
type soperator = Union | Intersect

type pure_term = PFun of (cname * pure_term list) | PInt of int | PVar of vname

type set = Set of (element list * vdecls) | CallS of callable | ListS of (soperator * set * set list) | BigS of (soperator * vdecls * set) | Setminus of (set * set list)
and vdecls = vdecl list
and vdecl = FromSet of (pure_term * set) | Constraint of constraints
and constraints = In of (term * set) | Notin of (term * set) | Relation of (roperator * term * term)
and term = Exp of expr | Fun of (cname * term list) | Var of vname
and element = Tuple of term | Range of (expr * expr)
and expr = VarE of vname | Int of int | ListE of (eoperator * expr * expr list) | BigE of (eoperator * vdecls * expr) | Subtract of (expr * expr list)
and callable = Call of (cname * term list) | VarC of vname

type formula = CallF of callable | Const of coperator | Neg of formula | ListF of (foperator * formula * formula list) | BigF of (foperator * vdecls * formula) | Modal of (moperator * program * formula)
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (poperator * program * program list) | BigP of (poperator * vdecls * program) | Converse of program | Kleene of program

type 'a decl = vdecls * callable * 'a
type file = set decl list * formula decl list * program decl list * callable
end end

module CIRCUIT = struct module T = struct
type ground_term = Fun of (AST.T.cname * ground_term list) | Int of int
type callable = AST.T.cname * ground_term list
type formula = CallF of callable | Const of AST.T.coperator | Neg of formula | ListF of (AST.T.foperator * formula * formula list) | Modal of (AST.T.moperator * program * formula)
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (AST.T.poperator * program * program list) | Converse of program | Kleene of program

type 'a decl = callable * 'a
type file = formula decl list * program decl list * callable
end end

module FORMULA = struct module T = struct
type formula = CallF of (bool * CIRCUIT.T.callable) | Const of AST.T.coperator | ListF of (AST.T.foperator * formula list) | Modal of (AST.T.moperator * program * formula)
and program  = Assign of (CIRCUIT.T.callable * formula) | Test of formula | ListP of (AST.T.poperator * program list) | Converse of program | Kleene of program
end end
