open Syntax.SYNTAX

module KERNEL = struct

type sequent = {left : formula list; right : formula list};;

type theorem = sequent;; (*suggestion du sujet, feel free to choose better*)

exception Invalid;;

let rec _rev acc = function [] -> acc | b::tl -> _rev (b::acc) tl;;
let rev = _rev [];;
let insert p e l =
  let rec _boucle acc i  = function [] when (i > p)-> rev acc |[] -> rev (e::acc)
                                    |a::tl when (i == p)-> _boucle (a::e::acc) (i + 1) tl
                                    |a::tl -> _boucle (a::acc) (i + 1) tl in
 _boucle [] 0 l;;

let sel_left (th:sequent) (pos:int) (principal:formula) = if (pos < ((List.length th.left) + 1)) then
                                  let l = insert pos principal th.left in
                                  {right = l; left = th.right}
                                else failwith "Invalid"
let sel_right th pos principal = if (pos < ((List.length th.right) +1)) then
                                   let l = insert pos principal th.right in
                                   {right = l; left = th.left}
                                 else failwith "Invalid"

let and_left th = match th.left with [] -> failwith "Invalid" |a::[] -> failwith "Invalid"
                                     |a::b::tl -> ({left = tl ; right = th.right}, and_formula a b);;

let and_right th1 th2 = match th1.right, th2.right with [],_->failwith "Invalid" |_,[]->failwith "Invalid"
                                                       |a::tl1,b::tl2->({left = th1.left; right = tl1}, and_formula a b)


let or_left th1 th2 = match th1.left, th2.left with [],_->failwith "Invalid" |_,[]->failwith "Invalid"
                                                    |a::tl1,b::tl2->({left = tl1; right = th1.right}, or_formula a b)


let or_right th = match th.right with [] -> failwith "Invalid" |a::[] -> failwith "Invalid"
                                          |a::b::tl -> ({left = th.left; right = tl}, or_formula a b)


let true_left th = (th, true_formula ());;
let false_right th = (th, false_formula ());;


let false_left th = (th, false_formula ());; (*J'ai encore des doutes sur celles-ci, je me laisse un peu de temps, mais je crois que je les ai typées correctememt*)
let true_right th = (th, true_formula ());;


let init_left s f = match s with {left = l_l ; right = l_r} -> ({left = l_l ; right = f::l_r}, f);;
let init_right s f = match s with {left = l_l ; right = l_r} -> {left = f::l_l ; right = l_r}, f);;


let implies_left th1 th2 = match th1.right, th2.left with [],_->failwith "Invalid" |_,[]->failwith "Invalid"
                                                          |a::tl1,b::tl2->({left=th1.left,right=th2.right},implies_formula a b)
;;

let implies_right th = match th.left, th.right with [],_->failwith "Invalid" |_,[]->failwith "Invalid"
                                                          |a::tl1,b::tl2->({left=tl1,right=th2.right},implies_formula a b)
;;

let conclusion th = th;;

end
