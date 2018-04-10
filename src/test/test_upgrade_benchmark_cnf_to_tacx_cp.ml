let file_in = Sys.argv.(1)
and file_out = Sys.argv.(2);;

open StrLoadCnf
open IoUtils

let my_cnf = load (Stream.of_channel (open_in file_in));;

(* STEP 2: from CNF to Cp.TACX *)

let upgrade mymodule =
	let man = Cp.TACX.newman() in
	let ( &! ) = Cp.( &! ) man in
	let cneg = Cp.cno in
	let inputs = Oops.array_make_n_var man mymodule.input in
	let eval clauses = Oops.list_et (&!) (List.map (fun clause -> Oops.list_or (&!) (List.map (fun (b, x) -> cneg b inputs.(x)) clause)) clauses) in
	{
		tacx_name = "CNF";
		tacx_man  = man;
		tacx_input= MyList.init mymodule.input string_of_int;
		tacx_edges= ["OUT", eval mymodule.clauses]
	}
;;

let my_tacx = upgrade my_cnf;;

Cp.TACX.dumpfile my_tacx.tacx_man (List.map (fun (name, edge) -> edge) my_tacx.tacx_edges) file_out;;

exit 0;;
