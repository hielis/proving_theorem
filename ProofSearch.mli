open Kernel.KERNEL
open Forest.FOREST

module PROOFSEARCH : sig

val search : sequent -> int -> theorem * proofTree
val main : unit -> unit

end
