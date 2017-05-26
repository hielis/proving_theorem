open Syntax.SYNTAX

module GENERICS : sig

val is_equal : SYNTAX.formula -> SYNTAX.formula -> boolean
val contains_formula : SYNTAX.formula -> SYNTAX.formula list -> boolean

end
