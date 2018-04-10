let man = Cnax.CNAX.G1.newman();;

module OOPS = Cnax.OOPS(struct let cnax = man end);;
open OOPS.M0
open OOPS.VUInt

let start = Sys.time() ;;

let n = int_of_string Sys.argv.(1);;
let file_out =
	if Array.length  Sys.argv > 2
	then Sys.argv.(2)
	else ("workdir/"^(string_of_int n)^"-tcqueens.v");;


print_string "------ begin computation ------";;
print_newline();;

(* [c] denote the main constraint *)

let x, c = range n;;
let c = OOPS.array_and (OOPS.array_copy_fun_t n c);;
let xx = Array.init n ((+?/) x);;
let xxy = Array.map (copy_t n) xx;;
let xy = copy_t n x;;
let arity = xy.(0).arity;;


let matD = Array.init n (fun x -> Array.init x (fun y ->
	assert(y < x);
	let h = x - y in
	let a = xxy.(h).(x) <>/ xxy.(0).(y)
	and b = xxy.(h).(y) <>/ xxy.(0).(x) in
	(a &! b)
));;

let diags = OOPS.array_and (MyArray.flatten matD);;

let matL = Array.init n (fun x -> Array.init x (fun y ->
	assert(y < x);
	(xy.(x) <>/ xy.(y))
));;

let lines = OOPS.array_and (MyArray.flatten matL);;

let n_queens = c &! lines &! diags;;



print_string "------ end of computation ------";;
print_newline();;

print_string "time:\t";;
print_float (Sys.time() -. start);;
print_string " s.";;
print_newline();;

let mexpr = IoUtils.module_expr_of_cnax "tcqueens" arity man [|n_queens.edge|];;
StrDumpVerilog.dump_file file_out mexpr;;

exit 0;;
