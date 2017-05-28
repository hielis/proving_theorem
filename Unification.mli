open Syntax.SYNTAX

module UNIFICATION : sig

val unify_formula : formula -> formula -> (formula * formula)
exception UnificationImpossible

end
