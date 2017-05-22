module SYNTAX = struct
type term =
| Variable of string
| Constant of string
| Operator of string * term list
type formula =
| Predicate of string * term list
| And of formula * formula | True
| Or of formula * formula | False
| Implies of formula * formula
| Forall of string * formula
| Exists of string * formula

let predicate s l  = Predicate(s , l);; (*Juste pour pouvoir compiler ; A IMPLEMENTER*)
let operator s l = Operator(s, l);;

(*J'ai implementé les regles de De Morgan par defaut, je ne sais pas si c'est vraiment utile*)

let and_formula f1 f2 = match f1, f2 with _, False -> False |False,_ -> False  |True, True -> True |_,_ -> And(f1, f2);;
let or_formula f1 f2 = match f1, f2 with _, True -> True |True, _ -> True |False, False -> False |_,_ -> Or(f1, f2);;
let implies_formula f1 f2 = match f1,f2 with False, _ -> f2 | True, True -> True | True, False -> False | _,_ -> Implies(f1, f2);;

let true_formula () = True;;
let false_formula () = False;;

let rec is_equal f1 f2 =
  match f1,f2 with
  |Predicate(s1,_),Predicate(s2,_)->String.equal s1 s2
  |And(f11,f12),And(f21,f22)->((is_equal f11 f21) && (is_equal f12 f22))||((is_equal f11 f22) && (is_equel f12 f 21))
  |Or(f11,f12),Or(f21,f22)->((is_equal f11 f21) && (is_equal f12 f22))||((is_equal f11 f22) && (is_equel f12 f 21))
  |Implies(f11,f12),Implies(f21,f22)->(is_equal f11 f21) && (is_equal f12 f22)
;;

let rec contains_formula f = function
  |[]->false
  |t::q->(is_equal t f) || (contains_formula f q)
;;

end
