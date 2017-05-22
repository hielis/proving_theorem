module type SYNTAX = sig
type term
type formula

val operator : string -> term list -> term
val predicate : string -> term list -> term

end
