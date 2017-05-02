module type KERNEL = sig
type sequent = {
left : formula list ;
right : formula list ;
}
type theorem
exception Invalid
val conclusion : theorem -> sequent
val sel_left : theorem -> pos:int -> principal:formula -> theorem
val sel_right : theorem -> pos:int -> principal:formula -> theorem
val init_left : sequent -> principal:formula -> theorem
val init_right : sequent -> principal:formula -> theorem
val and_left : theorem -> theorem * formula
val true_left : theorem -> theorem * formula
(* and so on ... *)
end
