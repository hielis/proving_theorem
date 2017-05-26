open Syntax.SYNTAX
open Kernel.KERNEL

module PRINTER : sig

val sequent_to_string : sequent -> string
val theorem_to_string : theorem -> string
val print_sequent : sequent -> unit
val print_theorem : theorem -> unit

end
