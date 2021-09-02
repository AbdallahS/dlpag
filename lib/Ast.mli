include module type of Types.AST with module T = Types.AST.T

module Print :
sig
  val cname : T.cname -> string
  val callable : T.callable -> string
  val coperator : T.coperator -> string
  val eoperator : T.eoperator -> string
  val foperator : T.foperator -> string
  val moperator : T.moperator -> string -> string -> string
  val poperator : T.poperator -> string
  val expr : T.expr -> string
  val file : T.file -> string
end
