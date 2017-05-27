
open Syntax.SYNTAX


module KERNEL = struct

type sequent = {left : formula list; right : formula list};;

type theorem = sequent;; (*suggestion du sujet, feel free to choose better*)


exception Invalid;;
exception UnificationImpossible;;

let conclusion (t: theorem) = (t:sequent);;

let null_theorem () = {left = [] ; right = []};;

let rec _rev acc = function [] -> acc | b::tl -> _rev (b::acc) tl;;
let rev = _rev [];;
let insert p e l =
  let rec _boucle acc i  = function [] when (i > p)-> rev acc |[] -> rev (e::acc)
                                    |a::tl when (i == p)-> _boucle (a::e::acc) (i + 1) tl
                                    |a::tl -> _boucle (a::acc) (i + 1) tl in
 _boucle [] 0 l;;

let sel_left (th:sequent) (pos:int) (principal:formula) = if (pos < ((List.length th.left) + 1)) then
                                  let l = insert pos principal th.left in
                                  {left = l; right = th.right}
                                else failwith "InvalidSelLeft";;

let sel_right th pos principal = if (pos < ((List.length th.right) +1)) then
                                   let l = insert pos principal th.right in
                                   {right = l; left = th.left}
                                 else failwith "InvalidSelRight";;

let and_left th = match th.left with [] -> failwith "InvalidAndL" |a::[] -> failwith "InvalidANdL"
                                     |a::b::tl -> ({left = tl ; right = th.right}, and_formula a b);;

let and_right th1 th2 = match th1.right, th2.right with [],_->failwith "InvalidANdR" |_,[]->failwith "InvalidAndR"
                                                       |a::tl1,b::tl2->({left = th1.left; right = tl1}, and_formula a b);;


let or_left th1 th2 = match th1.left, th2.left with [],_->failwith "InvalidOrL" |_,[]->failwith "InvalidOrL"
                                                    |a::tl1,b::tl2->({left = tl1; right = th1.right}, or_formula a b);;


let or_right th = match th.right with [] -> failwith "InvalidOrR" |a::[] -> failwith "InvalidOrR"
                                          |a::b::tl ->({left = th.left; right = tl}, or_formula a b);;



let true_left th = (th, true_formula ());;
let false_right th = (th, false_formula ());;


let false_left th = (th, false_formula ());; (*J'ai encore des doutes sur celles-ci, je me laisse un peu de temps, mais je crois que je les ai typées correctememt*)
let true_right th = (th, true_formula ());;


let init_left s f = (s,f);;
let init_right s f =(s,f);;


let implies_left th1 th2 = match th1.right, th2.left with [],_->failwith "InvalidIL1" |_,[]->failwith "InvalidIL2"
                                                          |a::tl1,b::tl2->({left=th1.left;right=th2.right},implies_formula a b)
;;

let implies_right th = match th.left, th.right with [],_->failwith "InvalidIR1" |_,[]->failwith "InvalidIR2"
                                                          |a::tl1,b::tl2->({left=tl1;right=tl2},implies_formula a b)
;;

let rec concat_or b = function
  |[] -> b
  |hd::tl -> concat_or (b || hd) tl;;


let rec is_present_in s = function
  |Variable(sp) -> String.equal sp s
  |Constant(sp) -> false
  |Operator(sp, l) -> concat_or false (List.map (is_present_in s) l);;

let rec unify_term a t = match a, t with
  |Variable(s), _ when (is_present_in s t) -> raise UnificationImpossible
  |Variable(s),_ -> (s, t)
  |_, Variable(s) -> (s, a)
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
  |_ -> failwith "Reduction failed and Unification was called too soon" in

let rec aux3 f1 f2 = function
  |[] -> (f1, f2)
  |hd::tl -> let (s, t) = hd in
             aux3 (replace_formula s t f1) (replace_formula s t f2) tl in

aux3 f1 f2 (List.rev (aux2 f1 f2));;

end
