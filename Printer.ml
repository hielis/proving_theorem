open SYNTAX
open KERNEL

module PRINTER = struct

let rec term_to_string = function
  |Variable(s)-> ("Var("^(s^")"))
  |Constant(s)-> ("Cons("^(s^")"))
  |Operator(s,l)-> (s^"(")^((term_list_to_string l)^")")
and  _tlts_aux = function
  |[]->""
  |t::q->(" , "^(term_to_string t))^(_tlts_aux q)
and term_list_to_string = function
     |[]->""
     |t::q->(term_to_string t)^(_tlts_aux q)
;;

let rec formula_to_string = function
Predicate(s,l)->((s^"(")^(term_list_to_string l))^")"
  |Or(f1,f2)->("("^(formula_to_string f1))^(" \\lor "^((formula_to_string f2)^")"))
  |And(f1,f2)->("("^(formula_to_string f1))^(" \\land "^((formula_to_string f2)^")"))
  |Implies(f1,f2)->("("^(formula_to_string f1))^(" \\Rightarrow "^((formula_to_string f2)^")"))
  |True-> "\\top" |False-> "\\bot"
  |Forall(s,f)->("\\left[\\forall "^(s^", "))^((formula_to_string f)^"\\right]")
  |Exists(s,f)->("\\left[\\exists "^(s^", "))^((formula_to_string f)^"\\right]")
;;

let rec _flts_aux = function
  |[]->""
  |t::q->(" , "^(formula_to_string t))^(_flts_aux q)
;;

let formula_list_to_string = function
|[]->"()";
|t::q->(formula_to_string t)^(_flts_aux q)
;;

let sequent_to_string ccl  = ((formula_list_to_string ccl.left)^" \\vdash ")^(formula_list_to_string ccl.right)
;;

let theorem_to_string ccl = sequent_to_string (conclusion ccl);;

let print_sequent ccl = (Pervasives.print_string (sequent_to_string ccl); Pervasives.print_newline ());;

let print_theorem ccl = print_sequent (conclusion ccl);;
end
