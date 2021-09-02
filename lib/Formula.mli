include module type of Types.FORMULA with module T = Types.FORMULA.T

module Print : sig
  val formula : T.formula -> string
  val program : T.program -> string
end


val extract_atoms : T.formula -> Circuit.T.callable list
val file : Circuit.T.file -> T.formula
