open Kernel.KERNEL
open Forest.FOREST

module PROOFSEARCH : sig

val search : sequent -> int -> theorem * proofTree
val search_print : sequent -> unit
val main : unit -> unit

end
