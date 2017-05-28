open Syntax.SYNTAX

module GENERICS = struct

let rec is_equal f1 f2 =
  match f1,f2 with
  |Predicate(s1,_),Predicate(s2,_)->String.equal s1 s2 (*A priori, UNIFIER ICI*)
  |And(f11,f12),And(f21,f22)->((is_equal f11 f21) && (is_equal f12 f22))||((is_equal f11 f22) && (is_equal f12 f21))
  |Or(f11,f12),Or(f21,f22)->((is_equal f11 f21) && (is_equal f12 f22))||((is_equal f11 f22) && (is_equal f12 f21))
  |Implies(f11,f12),Implies(f21,f22)->(is_equal f11 f21) && (is_equal f12 f22)
  |_,_->false
;;

let rec contains_formula f = function
  |[]->false
  |t::q->(is_equal t f) || (contains_formula f q)
;;

end
