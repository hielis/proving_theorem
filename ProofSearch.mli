open Kernel.KERNEL

module PROOFSEARCH : sig

val search : KERNEL.sequent -> int -> KERNEL.theorem * KERNEL.proofTree

end
