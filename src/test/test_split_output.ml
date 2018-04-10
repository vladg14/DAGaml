open IoUtils

let file_in = Sys.argv.(1) ^ Sys.argv.(2);;
let mexpr = StrLoadVerilog.load_file file_in;;
let cnax, edges = cnax_of_module_expr mexpr;;
List.iteri (fun idx edge ->
	let mexpr = module_expr_of_cnax
		mexpr.expr_name
		(List.length mexpr.expr_input)
		cnax
		[|edge |> snd|] in
	let name = Sys.argv.(1)^"-"^(string_of_int idx)^Sys.argv.(2) in
	StrDumpVerilog.dump_file name mexpr) edges;;
