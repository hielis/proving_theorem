module SYNTAX = struct
type term = private
| Variable of string
| Constant of string
| Operator of string * term list
type formula = private
| Predicate of string * term list
| And of formula * formula | True
| Or of formula * formula | False
| Implies of formula * formula
| Forall of string * formula
| Exists of string * formula

let dummy_predicate = function a::b::[] -> Predicate("dummy",[a; b]) | _ -> failwith "Aie !";;
end
