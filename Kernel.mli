open Syntax.SYNTAX

module KERNEL : sig
type sequent = {
left : Syntax.SYNTAX.formula list ;
right : formula list}
type theorem
exception Invalid
val print_sequent : sequent -> unit
val _rev : 'a list -> 'a list -> 'a list
val conclusion : theorem -> sequent
val sel_left : theorem -> pos:int -> principal:SYNTAX.formula -> theorem (*done*)
val sel_right : theorem -> pos:int -> principal:SYNTAX.formula -> theorem (*done*)

val init_left : sequent -> principal:SYNTAX.formula -> theorem
val init_right : sequent -> principal:SYNTAX.formula -> theorem

val and_left : theorem -> theorem * SYNTAX.formula (*done*)
val and_right : theorem -> theorem -> theorem * SYNTAX.formula (*done*)

val true_left : theorem -> theorem * SYNTAX.formula (*done*)
val true_right : sequent -> theorem * SYNTAX.formula

val false_left : sequent -> theorem * SYNTAX.formula
val false_right : sequent -> theorem * SYNTAX.formula (*done*)

val or_left : theorem -> theorem -> theorem * SYNTAX.formula (*done*)
val or_right : theorem -> theorem * SYNTAX.formula (*done*)

val implies_left : theorem -> theorem -> theorem * SYNTAX.formula
val implies_right : theorem -> theorem * SYNTAX.formula

end
