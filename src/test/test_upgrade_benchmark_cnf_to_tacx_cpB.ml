let file_in = Sys.argv.(1)
and file_out = Sys.argv.(2);;

open StrLoadCnf
open IoUtils

let my_cnf = load (Stream.of_channel (open_in file_in));;

(* STEP 2: from CNF to Cp.TACX *)

let upgrade mymodule =
	let man = CpB.TACX.G1.newman() in
	let ( &! ) edge0 edge1 = CpB.TACX.G1.push man (TacxTypes.And, edge0, edge1) in
	let cneg = CpB.cneg in
	let inputs = OopsB.array_make_n_var man mymodule.input in
	let eval clauses = OopsB.list_et (&!) (List.map (fun clause -> OopsB.list_or (&!) (List.map (fun (b, x) -> cneg b inputs.(x)) clause)) clauses) in
	{
		tacxB_name = "CNF";
		tacxB_man  = man;
		tacxB_input= MyList.init mymodule.input string_of_int;
		tacxB_edges= ["OUT", eval mymodule.clauses]
	}
;;

let my_tacx = upgrade my_cnf;;

CpB.TACX.dumpfile my_tacx.tacxB_man (List.map snd my_tacx.tacxB_edges) file_out;;

exit 0;;
