open Kernel.KERNEL
open Printer.PRINTER

module FOREST : sig

type proofTree

val computeUnary : string -> KERNEL.theorem -> proofTree -> proofTree
val computeBinary : string -> KERNEL.theorem -> proofTree -> proofTree -> proofTree
val leaf : unit -> proofTree

val tree_to_latex : proofTree -> unit 

end
