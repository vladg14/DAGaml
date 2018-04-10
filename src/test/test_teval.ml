open Extra

let main sigma file_in =
	let sigma = sigma
		|> StrUtil.explode
		||> StrUtil.bool_of_char
		|> Array.of_list in
	let mexpr = StrLoadVerilog.load_file file_in in
	let cnax, edges = IoUtils.cnax_of_module_expr mexpr in
	let edges = edges ||> snd |> Array.of_list in
	print_string (Cnax.CNAX.total_eval cnax sigma edges
		|> Array.to_list
		||> StrUtil.char_of_bool
		|> StrUtil.implode);
	print_newline();
	()

let _ = main Sys.argv.(1) Sys.argv.(2);;

exit 0;;
