
open Syntax.SYNTAX

module KERNEL : sig
type sequent = {
left : formula list ;
right : formula list}
type theorem

exception Invalid
exception UnificationImpossible

val null_theorem : unit -> theorem

val _rev : 'a list -> 'a list -> 'a list
val conclusion : theorem -> sequent
val sel_left : theorem -> int -> formula -> theorem
val sel_right : theorem ->int -> formula -> theorem

val init_left : sequent -> formula -> theorem * formula
val init_right : sequent -> formula -> theorem * formula

val and_left : theorem -> theorem * formula
val and_right : theorem -> theorem -> theorem * formula

val true_left : theorem -> theorem * formula
val true_right : sequent -> theorem * formula

val false_left : sequent -> theorem * formula
val false_right : theorem -> theorem * formula

val or_left : theorem -> theorem -> theorem * formula
val or_right : theorem -> theorem * formula

val implies_left : theorem -> theorem -> theorem * formula
val implies_right : theorem -> theorem * formula

val exists_right : (string -> formula -> formula) -> theorem -> string -> theorem * formula
val forall_left : (string -> formula -> formula) -> theorem -> string -> theorem * formula
val exists_left : (string -> formula -> formula) -> theorem -> string -> theorem * formula
val forall_right : (string -> formula -> formula) -> theorem -> string -> theorem * formula




end
