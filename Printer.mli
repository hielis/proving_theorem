open Syntax.SYNTAX
open Kernel.KERNEL

module PRINTER : sig

val sequent_to_string : KERNEL.sequent -> string
val theorem_to_string : KERNEL.theorem -> string
val print_sequent : KERNEL.sequent -> unit
val print_theorem : KERNEL.theorem -> unit

end
