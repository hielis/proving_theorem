open Syntax.SYNTAX;;

module REPLACEMENT : sig
val hash : int -> int -> int
val replace_term_by_variable_in_formula : string -> term -> formula -> formula * ((int * int * int) list)
val replace_term_by_position_in_formula : (int * int * int) -> term -> formula -> formula
val replacement_by_positions : term -> formula -> (int * int * int) list -> formula
end
= struct


type tree = F of int | N of int * (tree list);;

let hash n m = (((n + m) * (n + m + 1) / 2) + m);;

let rec loop i j = function |True-> F(hash i j) | False-> F(hash i j)
                              |Predicate(_, l)-> F(hash i j)
                              |Exists(_,f)-> N(hash i j, [loop (i + 1) 0 f])
                              |Forall(_,f)-> N(hash i j, [loop (i + 1) 0 f])
                              |And(f1, f2)-> N(hash i j, [loop (i + 1) 0 f1; loop (i + 1) 1 f2])
                              |Or(f1, f2)-> N(hash i j, [loop (i + 1) 0 f1; loop (i + 1) 1 f2])
                              |Implies(f1, f2)-> N(hash i j, [loop (i + 1) 0 f1; loop (i + 1) 1 f2]);;


let rec split l1 l2 = function [] -> (List.rev l1, l2) | (a, b)::tl -> split (a::l1) (l2@b) tl;;


let rec loop_term i j = function
  |Constant(s)-> F(hash i j)
  |Variable(s)-> F(hash i j)
  |Operator(s, l)-> N(hash i j, on_list i 0 l)
and on_list i acc = function [] -> [] | a::tl -> (loop_term i acc a)::(on_list i (acc + 1) tl);;

let replace_term_in_term m mp s t1 t2 =
  let tree = loop_term 0 0 t2 in
  let rec aux1 fp t = match fp, t with
    |Constant(s1), F(n) -> (Constant(s1), [])
    |Variable(s1), F(n) when (String.equal s1 s) -> (t1, [(m, mp, n)])
    |Variable(s1), F(n)-> (Variable(s1), [])
    |Operator(s1, l1), N(n, l2)-> let l = List.map2 aux1 l1 l2 in
                                  let l3, l4 = split [] [] l in
                                  (Operator(s1, l3), l4)
  in
  aux1 t2 tree;;

let replace_term_by_variable_in_formula s t f =
  let t1 = loop 0 0 f in
  let rec aux1 fp tp = match fp, tp with
    |True, _-> (True, []) |False, _ -> (False, [])
    |Predicate(s1, l), F(n)-> let rec aux2 i acc1 acc2 = function []-> (List.rev acc1, acc2) |a::tl-> let a, b = replace_term_in_term n i s t a in
      aux2 (i + 1) (a::acc1) (acc2@b) tl
                             in
                             let a,b = aux2 0 [] [] l in
                             (Predicate(s1, a), b)
    |Or(f1, f2), N(_, a::b::[]) -> let f1p, l1 = aux1 f1 a and f2p, l2 = aux1 f2  b in
                                   (Or(f1p, f2p), l1@l2)
    |And(f1, f2), N(_, a::b::[]) -> let f1p, l1 = aux1 f1 a and f2p, l2 = aux1 f2 b in (And(f1p, f2p), l1@l2)
    |Implies(f1, f2), N(_, a::b::[]) -> let f1p, l1 = aux1 f1 a and f2p, l2 = aux1 f2 b in (Implies(f1p, f2p), l1@l2)
    |Exists(s1, f1), F(_) when (String.equal s s1) -> failwith "Something went wrong with the terms, guys !"
    |Exists(s1, f1), N(_, a::[])-> let fp, l  = aux1 f1 a in
                                   (Exists(s1, fp), l)
    |Forall(s1, f1), F(_) when (String.equal s s1) -> failwith "Something went wrong with the terms, guys !"
    |Forall(s1, f1), N(_, a::[])-> let fp, l  = aux1 f1 a in
                                   (Forall(s1, fp), l)
  in
  aux1 f t1;;

let replace_term_by_position_in_term p t1 t2 =
  let tree = loop_term 0 0 t2 in
  let rec aux1 term tree = match term, tree with
    |_, N(n, _) when (n == p)-> t1
    |_, F(n) when (n == p)-> t1
    |_, F(n) -> term
    |Operator(s1, l1), N(n, l2) -> let l = List.map2 aux1 l1 l2 in
                                   Operator(s1, l)
  in
  aux1 t2 tree;;

let replace_term_by_position_in_formula p t1 f =
  let tree = loop 0 0 f in
  let n, i, m = p in
   let rec aux1 fp tp = match fp, tp with
    |Predicate(s1, l), F(n1) when (n == n1) -> let rec aux2 j acc = function [] -> List.rev acc
                                                                        |a::tl when (i == j)-> aux2 (j + 1) ((replace_term_by_position_in_term m t1 a)::acc) tl
                                                                        |a::tl-> aux2 (j + 1) (a::acc) tl
                                               in
                                               Predicate(s1, aux2 0 [] l)
    |_, F(n1)-> fp
    |Or(f1, f2), N(_, a::b::[]) -> let f1p = aux1 f1 a and f2p  = aux1 f2  b in
                                   Or(f1p, f2p)
    |And(f1, f2), N(_, a::b::[]) -> let f1p = aux1 f1 a and f2p = aux1 f2 b in And(f1p, f2p)
    |Implies(f1, f2), N(_, a::b::[]) -> let f1p = aux1 f1 a and f2p = aux1 f2 b in Implies(f1p, f2p)
    |Exists(s1, f1), N(_, a::[])-> let fp = aux1 f1 a in
                                   Exists(s1, fp)
    |Forall(s1, f1), N(_, a::[])-> let fp = aux1 f1 a in
                                   Forall(s1, fp)
  in
  (aux1 f tree);;

let rec replacement_by_positions t1 f = function []-> f | a::tl-> replacement_by_positions t1 (replace_term_by_position_in_formula a t1 f) tl;;

end
