open SYNTAX

open KERNEL (* Kernel.KERNEL
open Syntax.SYNTAX*)

module PROOFSEARCH = struct

exception Fail


let rec search conclusion bound =
(print_sequent conclusion;
  if (bound<0) then (Pervasives.print_string "Neg Bound";Pervasives.print_newline (); raise Fail;)
    else
       let rec try_right_principal i l = function
        |[]->raise Fail
        |t::q->if (contains_formula t conclusion.left) then
               let (a,b) = init_right {left = conclusion.left; right = (_rev q l)} t in let th = sel_right a i b in (th, computeUnary "initR" th (leaf ()))
               else
               let fail ()=try_right_principal (i+1) (t::l) q in
               match t with
               |True->let (a,b)=true_right {left= conclusion.left; right = (_rev q l)} in let th = sel_right a i b in (th, computeUnary "TrueR" th (leaf ()))
               |False->let succeed ccl tree =
                         let (a,b)=false_right ccl
                         in let th = sel_right a i b
                         in (th, computeUnary "FalseR" th tree)
                       in search_aux {left= conclusion.left; right = (_rev q l)} (bound-1) succeed fail
               |Or(f1,f2)->let succeed ccl tree =
                              let (a,b)=or_right ccl
                              in let th = sel_right a i b
                              in (th, computeUnary "or-R" th tree)
                           in search_aux {left= conclusion.left; right = f1::f2::(_rev q l)} (bound-1) succeed fail
               |And(f1,f2)->let l_aux = (_rev q l)
                               in let succeed1 ccl1 tree1 =
                                  let succeed2 ccl2 tree2 =
                                     let (a,b)=(and_right ccl1 ccl2)
                                     in let th = sel_right a i b
                                     in (th, computeBinary "and-R" th tree1 tree2)
                                  in search_aux {left= conclusion.left; right = f2::l_aux} (bound-1) succeed2 fail
                               in search_aux {left=conclusion.left; right = f1::l_aux} (bound-1) succeed1 fail
               |Implies(f1,f2)->let succeed ccl tree =
                                   let (a,b)=implies_right ccl
                                   in let th = sel_right a i b
                                   in (th, computeUnary "==>R" th tree)
                                in search_aux {left= f1::conclusion.left; right = f2::(_rev q l)} (bound-1) succeed fail
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
                         in (th, computeUnary "TrueL" th tree)
                      in search_aux {left= (_rev q l); right = conclusion.right} (bound-1) succeed fail
               |False->let (a,b)=false_left {left= (_rev q l); right = conclusion.right} in let th = sel_left a i b in (th, computeUnary "FalseL" th (leaf ()))
               |And(f1,f2)->let succeed ccl tree  =
                               let (a,b)=and_left ccl
                               in let th = sel_left a i b
                               in (th, computeUnary "and-L" th tree)
                            in search_aux {left= f1::f2::(_rev q l); right = conclusion.right} (bound-1) succeed fail
               |Or(f1,f2)-> let l_aux =(_rev q l)
                            in let succeed1 ccl1 tree1 =
                               let succeed2 ccl2 tree2 =
                                 let (a,b)=(or_left ccl1 ccl2)
                                 in let th = sel_left a i b in (th, computeBinary "or-L" th tree1 tree2)
                               in search_aux {left= f2::l_aux; right = conclusion.right} (bound-1) succeed2 fail
                            in search_aux {left=f1::l_aux; right = conclusion.right} (bound-1) succeed1 fail
               |Implies(f1,f2)-> let l_aux =(_rev q l)
                                 in let succeed1 ccl1 tree1 =
                                    let succeed2 ccl2 tree2 =
                                       let (a,b)=(implies_left ccl1 ccl2)
                                       in let th = sel_left a i b in (th, computeBinary "==>L" th tree1 tree2)
                                    in search_aux {left= f2::l_aux; right = conclusion.right} (bound-1) succeed2 fail
                                 in search_aux {left=l_aux; right = f1::conclusion.right} (bound-1) succeed1 fail
               |_->fail ()

     in try_left_principal 0 [] conclusion.left)

and search_aux conclusion bound succeed fail =
  try let (th,tree) = search conclusion bound in succeed th tree with Fail->fail () ;;

let s1 = {left = [False]; right = [Predicate("P",[])]};;
let s2 = {left = []; right = [Or(Predicate("p",[]),Implies(Predicate("p",[]),False))]};;
let s3 = {left = []; right = [Implies(Implies(Implies(Predicate("p",[]),Predicate("q",[])),Predicate("p",[])),Predicate("p",[]))]};;
let s4 = {left = []; right=[Implies(Implies(Implies(Predicate("p",[]),False),False),Predicate("p",[]))]};;
let s5 = {left = []; right=[Implies(Implies(Predicate("p",[]),Implies(Predicate("q",[]),Predicate("r",[]))),Implies(Implies(Predicate("p",[]),Predicate("q",[])),Implies(Predicate("p",[]),Predicate("r",[]))))]};;
let s6 = {left=[]; right=[Implies(Implies(And(Predicate("p",[]),Predicate("q",[])),False),Or(Implies(Predicate("p",[]),False),Implies(Predicate("q",[]),False)))]};;
let s7 = {left = []; right=[Implies(Or(And(Predicate("p1",[]),Predicate("q1",[])),And(Predicate("p2",[]),Predicate("q2",[]))),And(Or(Predicate("p1",[]),Predicate("p2",[])),Or(Predicate("q1",[]),Predicate("q2",[]))))]};;

let main () = let a = {left = [True]; right = [False]} in
                let rec aux i = try (search s2 i) with Fail->(Pervasives.print_string "Failed"; Pervasives.print_newline ();if i<1000 then aux (i+1) else ({left=[];right=[]}, leaf () )) in aux 0;;

end;;

PROOFSEARCH.main ();;

search  {left = [Predicate("p",[])]; right = [False;Predicate("p",[])]} 100;;

contains_formula (Predicate("p",[])) ([False;Predicate("p",[])]);;
