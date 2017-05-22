open Kernel.KERNEL
open Syntax.SYNTAX

module PROOFSEARCH = struct

exception Fail


let rec search conclusion:sequent bound:int =
  if (bound<0) then raise Fail
    else begin
       let rec try_right_principal = function
        |[]->raise Fail
        |t::q->if (contains_formula t conclusion.left) then
               let (a,b) = init_right {left = conclusion.left; right = (_rev q l)} t in sel_right a i b
               else begin
               let fail ()=try_right_principal q in
               match t with
               |True->let (a,b)=true_right {left= conclusion.left; right = (_rev q l)} in sel_right a i b
               |False->let succeed ccl = let (a,b)=false_right ccl in sel_right a i b in search_aux {left= conclusion.left; right = (_rev q l)} (bound-1) succeed fail
               |Or(f1,f2)->let succeed ccl = let (a,b)=or_right ccl in sel_right a i b in search_aux {left= conclusion.left; right = f1::f2::(_rev q l)} (bound-1) succeed fail
               |And(f1,f2)->let succeed1 ccl1 = let l_aux =(_rev q l) in let succeed2 ccl2 = let (a,b)=(or_left ccl1 ccl2) in sel_right a i b in search_aux {left= conclusion.left; right = f2::l_aux} (bound-1) succeed2 fail in search_aux {left=conclusion.left; right = f1::l_aux} (bound-1) succeed1 fail
               |Implies(f1,f2)->let succeed ccl = let (a,b)=implies_right ccl in sel_right a i b in search_aux {left= f1::conclusion.left; right = f2::(_rev q l)} (bound-1) succeed fail
               end

      in let rec try_left_principal i l = function
        |[]->try_right_principal conclusion.right
        |t::q->if (contains_formula t conclusion.right) then
               let (a,b) = init_left {left = (_rev q l); right = conclusion.right} in sel_left a i b
               else begin
               let fail () = try_left_principal (i+1) t::l q in
               match t with
               |True->let succeed ccl = let (a,b)=true_left ccl in sel_left a i b in search_aux {left= (_rev q l); right = conclusion.right} (bound-1) succeed fail
               |False->let (a,b)=false_left {left= (_rev q l); right = conclusion.right} in sel_left a i b
               |And(f1,f2)->let succeed ccl = let (a,b)=and_left ccl in sel_left a i b in search_aux {left= f1::f2::(_rev q l); right = conclusion.right} (bound-1) succeed fail
               |Or(f1,f2)->let succeed1 ccl1 = let l_aux =(_rev q l) in let succeed2 ccl2 = let (a,b)=(or_left ccl1 ccl2) in sel_left a i b in search_aux {left= f2::l_aux; right = conclusion.right} (bound-1) succeed2 fail in search_aux {left=f1::l_aux; right = conclusion.right} (bound-1) succeed1 fail
               |Implies(f1,f2)->let succeed1 ccl1 = let l_aux =(_rev q l) in let succeed2 ccl2 = let (a,b)=(implies_left ccl1 ccl2) in sel_left a i b in search_aux {left= f2::l_aux; right = conclusion.right} (bound-1) succeed2 fail in search_aux {left=l_aux; right = f1::conclusion.right} (bound-1) succeed1 fail
               end
    end

and search_aux conclusion:sequent bound:int succeed fail =
  try succeed (search sequent bound) with Fail->fail ();;


end
