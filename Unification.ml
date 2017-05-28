open Syntax.SYNTAX;;

module UNIFICATION : sig
val unify_formula : formula -> formula -> (formula * formula)
exception UnificationImpossible

end =
struct
exception UnificationImpossible

let rec replace_term s t = function
  |Variable(sp) when (String.equal s sp) -> t
  |Variable(sp) -> Variable(sp)
  |Meta(sp, _) when (String.equal s sp) -> t
  |Meta(sp, l) -> Meta(sp, l)
  |Constant(sp) -> Constant(sp)
  |Operator(sp, l) -> Operator(sp, List.map (replace_term s t) l);;

let rec replace_formula s t = function
  |Predicate(sp, l) -> Predicate(sp, List.map (replace_term s t) l)
  |And(f1, f2) -> And(replace_formula s t f1, replace_formula s t f2)
  |Or(f1, f2) -> Or(replace_formula s t f1, replace_formula s t f2)
  |Implies(f1, f2) -> Implies(replace_formula s t f1, replace_formula s t f2)
  |True -> True
  |False -> False
  |Exists(sp, f) when (String.equal s sp) -> raise UnificationImpossible
  |Exists(sp, f) -> Exists(sp, replace_formula s t f)
  |Forall(sp, f) when (String.equal s sp) -> raise UnificationImpossible
  |Forall(sp, f) -> Forall(sp, replace_formula s t f);;

let rec concat_or b = function
  |[] -> b
  |hd::tl -> concat_or (b || hd) tl;;

let rec is_present_in s = function
  |Meta(sp, _) -> String.equal sp s
  |Variable(sp) -> String.equal sp s
  |Constant(sp) -> false
  |Operator(sp, l) -> concat_or false (List.map (is_present_in s) l);;




let rec unify_term a t =
  let condition l = function Constant(b)-> (List.mem b l) | _ -> false in
  match a, t with
  |Meta(s, l),_ when (condition l t) -> raise UnificationImpossible
  |Meta(s, l), _ when (is_present_in s t) -> raise UnificationImpossible
  |Meta(s, l),_ -> (s, t)
  |Variable(s), _ when (is_present_in s t) -> raise UnificationImpossible
  |Variable(s),_ -> (s, t)
  |_, Variable(s) -> (s, a)
  |_, Meta(s, l) when (condition l t) -> raise UnificationImpossible
  |_, Meta(s, l) -> (s, a)
  |_, _ -> raise UnificationImpossible;;

let unify_formula f1 f2 =
  let rec aux acc l1 l2 = match l1, l2 with
    |[], [] -> acc
    |t1::tl1, t2::tl2 -> (try (aux ((unify_term t1 t2)::acc) tl1 tl2) with _ -> aux acc tl1 tl2)
    |_,_ -> failwith "Predicates arguments are ill-sized" in

let rec aux2 f1 f2 =
  match f1, f2 with
  |Predicate(s1, l1), Predicate(s2, l2) when (not (String.equal s1 s2)) -> raise UnificationImpossible
  |Predicate(s1, l1), Predicate(s2, l2) -> aux [] l1 l2
  |_ -> raise UnificationImpossible in

let rec aux3 f1 f2 = function
  |[] -> (f1, f2)
  |hd::tl -> let (s, t) = hd in
             aux3 (replace_formula s t f1) (replace_formula s t f2) tl in

aux3 f1 f2 (List.rev (aux2 f1 f2));;
end
