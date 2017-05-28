open Syntax.SYNTAX
open Unification.UNIFICATION

module GENERICS = struct

let rec is_equal_term t1 t2 = match t1, t2 with
  |Variable(s1), Variable(s2)-> String.equal s1 s2
  |Meta(s1, _), Meta(s2, _)-> String.equal s1 s2
  |Constant(s1), Constant(s2)-> String.equal s1 s2
  |Operator(s1, l1), Operator(s2, l2)-> let rec aux b = function [] -> b | a::tl -> aux (b && a) tl in
                                    (String.equal s1 s2) && (try aux true (List.map2 is_equal_term l1 l2) with _ -> false)
  |_, _ -> false;;

let rec is_equal_list_term acc l1 l2 = match l1, l2 with
  |[],[] -> acc
  |_, [] -> false
  |[], _-> false
  |a::tl1, b::tl2 -> (is_equal_list_term ((is_equal_term a b)&&acc) tl1 tl2);;

let rec is_equal f1 f2 =
  match f1,f2 with
  |Predicate(s1,l1),Predicate(s2,l2)-> (String.equal s1 s2)&&(is_equal_list_term true l1 l2)
  |And(f11,f12),And(f21,f22)->((is_equal f11 f21) && (is_equal f12 f22))||((is_equal f11 f22) && (is_equal f12 f21))
  |Or(f11,f12),Or(f21,f22)->((is_equal f11 f21) && (is_equal f12 f22))||((is_equal f11 f22) && (is_equal f12 f21))
  |Implies(f11,f12),Implies(f21,f22)->(is_equal f11 f21) && (is_equal f12 f22)
  |_,_->false
;;

let rec contains_formula f = function
  |[]->false
  |t::q-> let f1, f2 = try unify_formula t f with UnificationImpossible -> (True, False) in
    (is_equal f1 f2) || (contains_formula f q);;

end
