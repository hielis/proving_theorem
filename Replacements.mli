open Syntax.SYNTAX

module REPLACEMENT : sig
val hash : int -> int -> int
val replace_term_by_variable_in_formula : string -> term -> formula -> formula * ((int * int * int) list)
val replace_term_by_position_in_formula : (int * int * int) -> term -> formula -> formula
val replacement_by_positions : term -> formula -> (int * int * int) list -> formula
val list_of_constants : formula list -> string list
end
