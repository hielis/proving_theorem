module SYNTAX = struct

exception UnificationImpossible

type term =
| Variable of string
| Constant of string
| Operator of string * term list;;

type formula =
| Predicate of string * term list
| And of formula * formula | True
| Or of formula * formula | False
| Implies of formula * formula
| Forall of string * formula
| Exists of string * formula;;

let predicate s l  = Predicate(s , l);; (*Juste pour pouvoir compiler ; A IMPLEMENTER*)
let operator s l = Operator(s, l);;

(*J'ai implementé les regles de De Morgan par defaut, je ne sais pas si c'est vraiment utile*)

let and_formula f1 f2 = match f1, f2 with _, False -> False |False,_ -> False  |True, True -> True |_,_ -> And(f1, f2);;
let or_formula f1 f2 = match f1, f2 with _, True -> True |True, _ -> True |False, False -> False |_,_ -> Or(f1, f2);;
let implies_formula f1 f2 = match f1,f2 with False, _ -> True | True, True -> True | True, False -> False | _,_ -> Implies(f1, f2);;

let true_formula () = True;;
let false_formula () = False;;

let rec replace_term s t = function
  |Variable(sp) when (String.equal s sp) -> t
  |Variable(sp) -> Variable(sp)
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

end
