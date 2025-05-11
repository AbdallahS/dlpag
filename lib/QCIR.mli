include module type of Types.QCIR with module T = Types.QCIR.T

module Print : sig
  val literal : T.literal -> string
  val operator : T.operator -> string
  val quant : T.quant -> string
  val gate_body : T.gate_body -> string
  val gate_statement : T.gate_statement -> string
  val qblock : T.qblock -> string
  val file : T.file -> string
end

val sanitize_names : T.file -> T.file
