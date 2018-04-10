open Cpx2Types
open Cpx2BUtils
open Cpx2BGops

module TSD = Cpx2BDump
module TO3 = Cpx2BO3


let opand = function
	| Some true , Some true  -> Some true
	| Some false, _
	| _         , Some false -> Some false
	| _                      -> None

let opxor : bool option * bool option -> bool option = function
	| Some x, Some y -> Some(x<>y)
	| _              -> None

let conserve_peval solver binop (((), edge0, edge1) as node01) =
	(* print_string "@@"; print_newline(); *)
	let arity = arity_edge edge0 in
	let npeval = Tools.math_pow 3 arity |> StrDump.int in
	assert(arity = arity_edge edge1);
	let merge = solver node01 in
	Iter.iter (fun peval ->
		let edge0'  = peval_pedge peval edge0
		and edge1'  = peval_pedge peval edge1
		and merge'  = peval_merge3 solver peval merge in
		let merge'' = solver ((), edge0', edge1') in
		if(not(merge' = merge''))
		then
		(
			print_string "\nlet peval   = ";
			GUtils.strdump_peval peval |> print_string;
			print_string " (* ";
			BinDump.(closure bool_option_list peval |> StrDump.bitv_hexa) |> print_string;
			print_string " *) ";
			print_string "\nand edge0   = ";
			TSD.edge edge0 |> print_string;
			print_string "\nand edge1   = ";
			TSD.edge edge1 |> print_string;
			print_string "\nand merge   = ";
			TSD.emerge3 merge |> print_string;
			print_string "\nand merge'  = ";
			TSD.emerge3 merge' |> print_string;
			print_string "\nand edge0'  = ";
			TSD.edge edge0' |> print_string;
			print_string "\nand edge1'  = ";
			TSD.edge edge1' |> print_string;
			print_string "\nand merge'' = ";
			TSD.emerge3 merge'' |> print_string;
			print_string " in";
			print_newline();
			assert(false)
		)
	) (GUtils.gen_peval (arity_edge edge0) |> Iter.progress ("prog("^npeval^"): ") 10000 0 )

let conserve_peval_elem solver binop (((), edge0, edge1) as node01) peval =
	(* print_string "@@"; print_newline(); *)
	let arity = arity_edge edge0 in
	assert(arity = arity_edge edge1);
	let merge = solver node01 in
	let edge0'  = peval_pedge peval edge0
	and edge1'  = peval_pedge peval edge1
	and merge'  = peval_merge3 solver peval merge in
	let merge'' = solver ((), edge0', edge1') in
	if(not(merge' = merge''))
	then
	(
		print_string "\nlet peval   = ";
		GUtils.strdump_peval peval |> print_string;
		print_string " (* ";
		BinDump.(closure bool_option_list peval |> StrDump.bitv_hexa) |> print_string;
		print_string " *) ";
		print_string "\nand edge0   = ";
		TSD.edge edge0 |> print_string;
		print_string "\nand edge1   = ";
		TSD.edge edge1 |> print_string;
		print_string "\nand merge   = ";
		TSD.emerge3 merge |> print_string;
		print_string "\nand merge'  = ";
		TSD.emerge3 merge' |> print_string;
		print_string "\nand edge0'  = ";
		TSD.edge edge0' |> print_string;
		print_string "\nand edge1'  = ";
		TSD.edge edge1' |> print_string;
		print_string "\nand merge'' = ";
		TSD.emerge3 merge'' |> print_string;
		print_string " in";
		print_newline();
		assert(false)
	)

let op = Sys.argv.(1)

let solver node = match op with
	| "A" -> solve_and node
	| "X" -> solve_xor node
	|  _  -> assert false

let binop = match op with
	| "A" -> opand
	| "X" -> opxor
	|  _  -> assert false

let aa = Sys.argv.(2)

let pnode = snd TO3.o3str_node' aa |> Utils.pnode_of_node;;

print_newline(); print_string (TSD.node pnode); print_newline();;

(*
if (Array.length Sys.argv > 3)
then
(
	let bb = Sys.argv.(3) in
	let peval = bb |> StrLoad.bitv_hexa |>  BinLoad.(closure bool_option_list) in
	GUtils.strdump_peval peval |> print_string; print_newline();
	conserve_peval_elem solver binop pnode peval;
	()
)
else (conserve_peval solver binop pnode; ());;
*)
exit 0;;
