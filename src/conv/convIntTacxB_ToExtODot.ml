module T = Cpx2B;;

open ConvTypes

let conv_stree_file modele stree fileB args =
	match modele with
	| Bryant -> failwith "WIP export .bryant.tacx to .dot"
	| Zdd    -> failwith "WIP export .zdd.tacx to .dot"
	| Cp     ->
	(
		let tacx, edges = CpB.TACX.stree_load stree in
		CpB.TACX.dotfile tacx edges fileB
	)
	| Nni    ->
		let tacx, edges = NniB.TACX.stree_load stree in
		NniB.TACX.dotfile tacx edges fileB
	| Cpx    ->
	(
		let tacx, edges = Cpx2B.TACX.stree_load stree in
		Cpx2B.TACX.dotfile tacx edges fileB
	)
