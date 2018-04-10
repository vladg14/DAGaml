let man = Cnax.CNAX.G1.newman();;
module OOPS = Cnax.OOPS(struct let cnax = man end);;
open OOPS.M0
open OOPS.VUInt

let n = int_of_string(Sys.argv.(1));;
let arity = 2 * n;;
let vect = OOPS.array_make_n_var arity;;
let x = {arity; array = Array.init n (fun i -> vect.(  i))}
and y = {arity; array = Array.init n (fun i -> vect.(n+i))} in

let c = x <=/ y;;
(* should be uniformly false *)

let mexpr = IoUtils.module_expr_of_cnax
	"exp_card"
	n
	man
	[|c.edge|]
in
StrDumpVerilog.dump_file Sys.argv.(2) mexpr;;

exit 0;;
