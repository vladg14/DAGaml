(* I) Header *)
(* Creates of a manager [man] to handle circuits with
		And, Xor, Not and Cons (i.e. shannon's operator). *)
let man = Cnax.CNAX.G1.newman();;
(* Instanciates the OPeratorS library of DAGaml with our
		manager *)
module OOPS = Cnax.OOPS(struct let cnax = man end);;
(* Opens the root module which defines operators:
	- And (&!)
	- Or  (|!)
	- Eq  (=!)
	- Xor (^!)
	- Not (neg)
*)
open OOPS.M0
(* Opens the Variable size Unsigned Integer library, which
		defines the following operators:
	- Addition (+/)
	- Equality (=/)
*)
open OOPS.VUInt

let main n k file_out =
	let x, c = range (k+1) in
	let c = OOPS.array_and (OOPS.array_copy_fun_t n c) in
	let nx = copy_t n x in
	let sum = nx |> array_add in
	let success = c &! (sum =/ (uint_of_int sum.arity k)) in
	let mexpr = IoUtils.module_expr_of_cnax
		"icp"
		success.arity
		man
		[|success.edge|]
	in
	StrDumpVerilog.dump_file file_out mexpr;
	()

let _ = main
	(int_of_string(Sys.argv.(1)))
	(int_of_string(Sys.argv.(2)))
	Sys.argv.(3)
