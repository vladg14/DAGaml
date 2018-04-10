open CnfTypes

let dump (output : string -> unit) (file:file) =
	output ("p cnf "^(string_of_int file.input)^" "^(string_of_int (List.length file.clauses))^"\n");
	List.iter (fun clause ->
		List.iter (fun (bool, var) -> output
			((string_of_int (if bool then -(var+1) else (var+1)))^" ")
		) clause;
		output "0\n"
	) file.clauses;
	()

let dump_file file cnf =
	let fout = open_out file in
	dump (output_string fout) cnf;
	close_out fout
