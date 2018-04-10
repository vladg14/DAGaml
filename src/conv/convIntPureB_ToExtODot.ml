module T = Cpx2B;;

open ConvTypes

let conv_stree_file modele stree fileB args =
	match modele with
	| Bryant -> failwith "WIP export .bryant.pure to .dot"
	| Zdd    -> failwith "WIP export .zdd.pure to .dot"
	| Cp     ->
	(
		let pure, edges = CpB.GroBdd.stree_load stree in
		CpB.GroBdd.dotfile pure edges fileB
	)
	| Nni    ->
		let pure, edges = NniB.GroBdd.stree_load stree in
		NniB.GroBdd.dotfile pure edges fileB
	| Cpx    ->
	(
		let pure, edges = Cpx2B.GroBdd.stree_load stree in
		Cpx2B.GroBdd.dotfile pure edges fileB
	)
