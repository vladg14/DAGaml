open ConvTypes
let conv_stree_file modele stree fileB args =
	let strman = Udag.String.newman () in
	let stredges = match modele with
	| Bryant -> failwith "WIP export .bryant.tacx to .dot"
	| Zdd    -> failwith "WIP export .zdd.tacx to .dot"
	| Cp ->
	(
		let tacx, edges = Cp.TACX.stree_load stree in
		Cp.TACX.to_dot tacx strman edges
	)
	| Nni ->
	(
		let tacx, edges = Nni.TACX.stree_load stree in
		Nni.TACX.to_dot tacx strman edges
	)
	| Cpx ->
	(
		let tacx, edges = Cpx.TACX.stree_load stree in
		Cpx.TACX.to_dot tacx strman edges
	)
	| _ -> assert false
	in
	Udag.String.to_dot_file strman stredges fileB
