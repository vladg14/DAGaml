let man = Cnax.CNAX.G1.newman();;
module OOPS = Cnax.OOPS(struct let cnax = man end);;
open OOPS.M0
open OOPS.VUInt

let n = int_of_string(Sys.argv.(1));;
let x = OOPS.array_make_n_var n;;

let c = exp_card n x <>/ exp_card' n x;;
(* should be uniformly false *)

let mexpr = IoUtils.module_expr_of_cnax
	"exp_card"
	n
	man
	[|c.edge|]
in
StrDumpVerilog.dump_file Sys.argv.(2) mexpr;;

exit 0;;
