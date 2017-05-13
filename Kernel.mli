open Syntax


module KERNEL : sig
type sequent = {
left : SYNTAX.formula list ;
right : SYNTAX.formula list}
type theorem
exception Invalid
val conclusion : theorem -> sequent
val sel_left : theorem -> pos:int -> principal:SYNTAX.formula -> theorem (*done*)
val sel_right : theorem -> pos:int -> principal:SYNTAX.formula -> theorem (*done*)

val init_left : sequent -> principal:SYNTAX.formula -> theorem
val init_right : sequent -> principal:SYNTAX.formula -> theorem

val and_left : theorem -> theorem * SYNTAX.formula (*done*)
val and_right : theorem -> theorem * SYNTAX.formula (*done*)

val true_left : theorem -> theorem * SYNTAX.formula (*done*)
val true_right : sequent -> theorem

val false_left : sequent -> theorem
val false_right : sequent -> theorem * SYNTAX.formula (*done*)

val or_left : theorem -> theorem * SYNTAX.formula (*done*)
val or_right : theorem -> theorem * SYNTAX.formula (*done*)
end
