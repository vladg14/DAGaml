open IoUtils

let conv_mexpr_to_cnf mexpr args =
	let man, edges = IoUtils.cnax_of_module_expr mexpr in
	let ninput = List.length mexpr.expr_input in
	Cnax.CNAX.export_cnf man ninput (List.map snd edges)
