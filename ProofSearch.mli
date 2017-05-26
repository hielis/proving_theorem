open Kernel.KERNEL
open Generics.GENERICS
open Printer.PRINTER
open Forest.FOREST

module PROOFSEARCH : sig

val search : KERNEL.sequent -> int -> KERNEL.theorem * FOREST.proofTree
val main : unit -> unit

end
