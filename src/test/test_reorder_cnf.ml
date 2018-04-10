open Extra

let cnf_in_f = Sys.argv.(1);;
let order_f = Sys.argv.(2);;
let cnf_out_f = Sys.argv.(3);;
let rev = if Array.length Sys.argv > 4
	then match Sys.argv.(4) with
		| "-rev" -> true
		| _      -> false
	else false;;
let file_order = open_in order_f;;
let remap = (
	let line = input_line file_order
		|> StrUtil.split ' ' in
	let line = if rev then List.rev line else line in
	let line = MyList.opmap
		(fun x -> try (Some (int_of_string x)) with _ -> None) line in
	Tools.inv_perm (Array.of_list line)
);;
let cnf_in = StrLoadCnf.load_file cnf_in_f;;
let cnf_out = CnfTypes.({
	input = cnf_in.input;
	clauses = List.map (fun c ->
		List.map (fun (b, x) -> (b, remap.(x))) c
	) cnf_in.clauses;
});;
StrDumpCnf.dump_file cnf_out_f cnf_out;;
