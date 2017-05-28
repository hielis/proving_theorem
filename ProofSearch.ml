open Syntax.SYNTAX
open Kernel.KERNEL
open Forest.FOREST
open Generics.GENERICS
open Unification.UNIFICATION
open Replacements.REPLACEMENT


module PROOFSEARCH = struct

exception Fail


let rec search conclusion bound =
  if (bound<0) then raise Fail
    else
       let rec try_right_principal i l = function
        |[]->raise Fail
        |t::q->if (contains_formula t conclusion.left) then
               let (a,b) = init_right {left = conclusion.left; right = (_rev q l)} t in let th = sel_right a i b in (th, computeUnary "initR" th (leaf ()))
               else
               let fail ()=try_right_principal (i+1) (t::l) q in
               match t with
               |True->let (a,b)=true_right {left= conclusion.left; right = (_rev q l)} in let th = sel_right a i b in (th, computeUnary "$\\top$ R" th (leaf ()))
               |False->let succeed ccl tree =
                         let (a,b)=false_right ccl
                         in let th = sel_right a i b
                         in (th, computeUnary "$\\bot$ R" th tree)
                       in search_aux {left= conclusion.left; right = (_rev q l)} (bound-1) succeed fail
               |Or(f1,f2)->let succeed ccl tree =
                              let (a,b)=or_right ccl
                              in let th = sel_right a i b
                              in (th, computeUnary "$\\lor$ R" th tree)
                           in search_aux {left= conclusion.left; right = f1::f2::(_rev q l)} (bound-1) succeed fail
               |And(f1,f2)->let l_aux = (_rev q l)
                               in let succeed1 ccl1 tree1 =
                                  let succeed2 ccl2 tree2 =
                                     let (a,b)=(and_right ccl1 ccl2)
                                     in let th = sel_right a i b
                                     in (th, computeBinary "$\\land$ R" th tree1 tree2)
                                  in search_aux {left= conclusion.left; right = f2::l_aux} (bound-1) succeed2 fail
                               in search_aux {left=conclusion.left; right = f1::l_aux} (bound-1) succeed1 fail
               |Implies(f1,f2)->let succeed ccl tree =
                                   let (a,b)=implies_right ccl
                                   in let th = sel_right a i b
                                   in (th, computeUnary "$\\Rightarrow$ R" th tree)
                                in search_aux {left= f1::conclusion.left; right = f2::(_rev q l)} (bound-1) succeed fail
               |Exists(s, f)-> let fp, lp = replace_term_by_variable_in_formula s (Variable(String.capitalize_ascii s)) f in
                               let succeed ccl tree =
                                 let r s f = replacement_by_positions (Variable(s)) f lp in
                                 let (a, b) = exists_right r ccl s in
                                 let th = sel_right a i b in
                                 (th, computeUnary "$\\exists$ R" th tree)
                               in search_aux {left = conclusion.left ; right = fp::(_rev q l)} (bound -1) succeed fail
               |Forall(s, f) when (not (List.mem s (list_of_constants ((_rev q l)@(conclusion.left))))) ->
                               let l_aux = (_rev q l) in
                               let l_constants = list_of_constants ((l_aux)@(conclusion.left)) in
                               let t = meta (String.capitalize_ascii s) (l_constants) in
                               let fp, lp = replace_term_by_variable_in_formula s t f in
                               let succeed ccl tree =
                                 let r s f = replacement_by_positions (Variable(s)) f lp in
                                 let (a, b) = forall_right r ccl s in
                                 let th = sel_left a i b in (th, computeUnary "$\\forall& R" th tree)
                               in search_aux {left = fp::l_aux; right = conclusion.right} (bound-1) succeed fail
               |_->fail ()


      in let rec try_left_principal i l = function
        |[]->try_right_principal 0 []  conclusion.right
        |t::q->if (contains_formula t conclusion.right) then
               let (a,b) = init_left {left = (_rev q l); right = conclusion.right} t in let th = sel_left a i b in (th, computeUnary "initL" th (leaf ()))
               else
               let fail () = try_left_principal (i+1) (t::l) q in
               match t with
               |True->let succeed ccl tree =
                         let (a,b)=true_left ccl
                         in let th = sel_left a i b
                         in (th, computeUnary "$\\top$ L" th tree)
                      in search_aux {left= (_rev q l); right = conclusion.right} (bound-1) succeed fail
               |False->let (a,b)=false_left {left= (_rev q l); right = conclusion.right} in let th = sel_left a i b in (th, computeUnary "$\\bot$ L" th (leaf ()))
               |And(f1,f2)->let succeed ccl tree  =
                               let (a,b)=and_left ccl
                               in let th = sel_left a i b
                               in (th, computeUnary "$\\land$ L" th tree)
                            in search_aux {left= f1::f2::(_rev q l); right = conclusion.right} (bound-1) succeed fail
               |Or(f1,f2)-> let l_aux =(_rev q l)
                            in let succeed1 ccl1 tree1 =
                               let succeed2 ccl2 tree2 =
                                 let (a,b)=(or_left ccl1 ccl2)
                                 in let th = sel_left a i b in (th, computeBinary "$\\lor$ L" th tree1 tree2)
                               in search_aux {left= f2::l_aux; right = conclusion.right} (bound-1) succeed2 fail
                            in search_aux {left=f1::l_aux; right = conclusion.right} (bound-1) succeed1 fail
               |Implies(f1,f2)-> let l_aux =(_rev q l)
                                 in let succeed1 ccl1 tree1 =
                                    let succeed2 ccl2 tree2 =
                                       let (a,b)=(implies_left ccl1 ccl2)
                                       in let th = sel_left a i b in (th, computeBinary "$\\Rightarrow$ L" th tree1 tree2)
                                    in search_aux {left= f2::l_aux; right = conclusion.right} (bound-1) succeed2 fail
                                 in search_aux {left=l_aux; right = f1::conclusion.right} (bound-1) succeed1 fail
               |Forall(s, f) -> let l_aux = (_rev q l) in
                                let fp, lp = replace_term_by_variable_in_formula s (Variable(String.capitalize_ascii s)) f in
                                let succeed ccl tree =
                                  let r s f = replacement_by_positions (Variable(s)) f lp in
                                  let (a, b) = exists_right r ccl s in
                                  let th = sel_left a i b in (th, computeUnary "$\\forall& L" th tree)
                                in search_aux {left = fp::l_aux; right = conclusion.right} (bound-1) succeed fail
               |Exists(s, f) when (not (List.mem s (list_of_constants ((_rev q l)@(conclusion.right))))) ->
                                let l_aux = (_rev q l) in
                                let l_constants = list_of_constants (l_aux@conclusion.right) in
                                let t = Meta(String.capitalize_ascii s, l_constants) in
                                let fp, lp = replace_term_by_variable_in_formula s t f in
                                let succeed ccl tree =
                                  let r s f = replacement_by_positions (Variable(s)) f lp in
                                  let (a, b) = exists_left r ccl s in
                                  let th = sel_left a i b in (th, computeUnary "$\\exists& L" th tree)
                                in search_aux {left = fp::l_aux; right = conclusion.right} (bound-1) succeed fail
               |_->fail ()

     in try_left_principal 0 [] conclusion.left

and search_aux conclusion bound succeed fail =
  try let (th,tree) = search conclusion bound in succeed th tree with Fail->fail () ;;

let s1 = {left = [False]; right = [Predicate("P",[])]};;
let s2 = {left = []; right = [Or(Predicate("p",[]),Implies(Predicate("p",[]),False))]};;
let s3 = {left = []; right = [Implies(Implies(Implies(Predicate("p",[]),Predicate("q",[])),Predicate("p",[])),Predicate("p",[]))]};;
let s4 = {left = []; right=[Implies(Implies(Implies(Predicate("p",[]),False),False),Predicate("p",[]))]};;
let s5 = {left = []; right=[Implies(Implies(Predicate("p",[]),Implies(Predicate("q",[]),Predicate("r",[]))),Implies(Implies(Predicate("p",[]),Predicate("q",[])),Implies(Predicate("p",[]),Predicate("r",[]))))]};;
let s6 = {left=[]; right=[Implies(Implies(And(Predicate("p",[]),Predicate("q",[])),False),Or(Implies(Predicate("p",[]),False),Implies(Predicate("q",[]),False)))]};;
let s7 = {left = []; right=[Implies(Or(And(Predicate("p1",[]),Predicate("q1",[])),And(Predicate("p2",[]),Predicate("q2",[]))),And(Or(Predicate("p1",[]),Predicate("p2",[])),Or(Predicate("q1",[]),Predicate("q2",[]))))]};;

let main () = let a = {left = [Exists("x", Forall("y", Predicate("p", [Variable("x") ; Variable("y")])))]; right = [Forall("y", Exists("x", Predicate("p", [Variable("x"); Variable("y")])))]} in
                let rec aux i = try (search a i) with Fail->if i<1000 then aux (i+1) else (null_theorem (), leaf ()) in let (th,tree) = aux 0 in tree_to_latex tree;;

end;;

PROOFSEARCH.main ();;

(*tree_to_latex (Unary("$\\forall$ L", {left = [Predicate("p",[])]; right = [False;Predicate("p",[]);Forall("x",Predicate("P",[Variable("x")]))]},Leaf));;

search  {left = [Predicate("p",[])]; right = [False;Predicate("p",[])]} 100;;

contains_formula (Predicate("p",[])) ([False;Predicate("p",[])]);;*)
