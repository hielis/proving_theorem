module type SYNTAX = sig
type term = private
            | Variable of string
            | Constant of string
            | Operator of string * term list

type formula = private
               | Predicate of string * term list
               | And of formula * formula | True
               | Or of formula * formula | False
               | Implies of fomula * formula
               | Forall of string * formula
               | Exists of string * formula

end
