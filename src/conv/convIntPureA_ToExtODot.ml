open ConvTypes
let conv_stree_file modele stree fileB args =
	let strman = Udag.String.newman () in
	let stredges = match modele with
	| Bryant ->
	(
		let pure, edges = Bryant.GroBdd.stree_load stree in
		Bryant.GroBdd.to_dot pure strman edges
	)
	| Zdd ->
	(
		let pure, edges = Zdd.GroBdd.stree_load stree in
		Zdd.GroBdd.to_dot pure strman edges
	)
	| Cp ->
	(
		let pure, edges = Cp.GroBdd.stree_load stree in
		Cp.GroBdd.to_dot pure strman edges
	)
	| Nni ->
	(
		let pure, edges = Nni.GroBdd.stree_load stree in
		Nni.GroBdd.to_dot pure strman edges
	)
	| Cpx ->
	(
		let pure, edges = Cpx.GroBdd.stree_load stree in
		Cpx.GroBdd.to_dot pure strman edges
	)
	in
	Udag.String.to_dot_file strman stredges fileB
