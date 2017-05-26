open Kernel.KERNEL
open Printer.PRINTER

module FOREST = struct

type proofTree = |Leaf |Unary of string * theorem * proofTree |Binary of string * theorem * proofTree * proofTree

let computeUnary s th t = Unary(s,th,t);;

let computeBinary s th t1 t2 = Binary(s,th,t1,t2);;

let leaf () = Leaf;;

let rec _ttl_aux = function
  |Leaf->"\\AxiomC{} "
  |Unary(s,th,tree)->((_ttl_aux tree)^(" \\RightLabel{\\scriptsize "^(s^"} ")))^("\\UnaryInfC{$"^((theorem_to_string th)^"$}"))
  |Binary(s,th,tree1,tree2)->(((_ttl_aux tree1)^(_ttl_aux tree2))^("\\RightLabel{\\scriptsize "^(s^"} ")))^("\\BinaryInfC{$"^((theorem_to_string th)^"$}"))
;;

let tree_to_latex tree =
  let head = "\\documentclass[10pt]{article} \\usepackage{bussproofs} \\usepackage{amssymb} \\usepackage{latexsym} \\begin{document} \\begin{prooftree} " in
  let tail = " \\end{prooftree} \\end{document}" in
  let latex_code = (head ^ (_ttl_aux tree))^tail in
  let latex_file = open_out "output/proof.tex" in
  let () = output_string latex_file latex_code in
  close_out latex_file
;;

end
