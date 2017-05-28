module SYNTAX : sig

exception UnificationImpossible

type term = (*private*)
| Meta of string * string list
| Variable of string
| Constant of string
| Operator of string * term list

type formula = (*private*)
| Predicate of string * term list
| And of formula * formula | True
| Or of formula * formula | False
| Implies of formula * formula
| Forall of string * formula
| Exists of string * formula

val predicate : string -> term list -> formula (*A IMPLEMENTER*)
val operator : string -> term list -> term (* A IMPLEMENTER*)
val constant : string -> term
val variable : string -> term
val meta : string -> string list -> term

val true_formula : unit -> formula
val and_formula : formula -> formula -> formula
val or_formula : formula -> formula -> formula
val implies_formula : formula -> formula -> formula

val true_formula : unit -> formula
val false_formula : unit -> formula

end
