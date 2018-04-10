let file_in = Sys.argv.(1)
and file_out = Sys.argv.(2);;

open IoUtils
open StrLoadVerilog

(* STEP 1: verilog parsing [START]*)

let my_verilog = StrLoadVerilog.load (Stream.of_channel (open_in file_in));;
(* STEP 1: PLA parsing [DONE] *)


(* STEP 2: from PLA to Cp.TACX *)

let upgrade mymodule =
	let man = Cp.TACX.newman() in
(*	let ( *! ) = Cp.( *! ) man *)
	let ( &! ) = Cp.( &! ) man
	and ( ^! ) = Cp.( ^! ) man in
	let neg = Cp.no in
	let inputs = Oops.list_make_n_var man (List.length mymodule.input) in
	let assoc = Hashtbl.create 10000 in
	List.iter2 (fun name func -> Hashtbl.add assoc name func) mymodule.input inputs;
	let rec eval = function
		| PVar ident -> Hashtbl.find assoc ident
		| PUop (uop, expr) ->
		(
			let func = eval expr in
			match uop with
			| PNop ->     func
			| PNot -> neg func 
		)
		| PBop (bop, exprX, exprY) ->
		(
			let funcX = eval exprX
			and funcY = eval exprY in
			match bop with
			| PAnd  -> funcX &! funcY
			| POr	-> neg((neg funcX)&!(neg funcY))
			| PImp	-> neg(funcX &! (neg funcY))
			| PIff	-> neg(funcX ^! funcY)
			| PXor	-> funcX ^! funcY
		)
	in
	List.iter (fun (name, expr) -> Hashtbl.add assoc name (eval expr)) mymodule.assign;
	let edges = List.map (fun name -> (name, Hashtbl.find assoc name)) mymodule.output in
	{
		tacx_name = mymodule.name;
		tacx_man  = man;
		tacx_input= mymodule.input;
		tacx_edges= edges
	}
;;

let my_tacx = upgrade my_verilog;;

Cp.TACX.dumpfile my_tacx.tacx_man (List.map (fun (name, edge) -> edge) my_tacx.tacx_edges) file_out;;

exit 0;;
