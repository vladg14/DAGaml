open ConvTypes
let conv_stree_file modele stree fileB args =
	let stree = match modele with
	| Bryant -> failwith "WIP export .bryant.tacx to stats"
	(*
	let tacx, edges = Bryant.TACX.stree_load stree in
	print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
	STree.pprint [Bryant.TACX.dump_stats tacx]
	*)
	| Zdd -> failwith "WIP export .zdd.tacx to stats"
	(*
	let tacx, edges = Zdd.TACX.stree_load stree in
	print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
	STree.pprint [Zdd.TACX.dump_stats tacx]
	*)
	| Cp ->
	(
		let tacx, edges = Cp.TACX.stree_load stree in
		Tree.Node[
			Tree.Leaf "#edges:";
			Tree.Node [STD.int (List.length edges)];
			Tree.Leaf "TACX:";
			Tree.Node [Cp.TACX.dump_stats tacx]
		]
	)
	| Nni ->
	(
		let tacx, edges = Nni.TACX.stree_load stree in
		Tree.Node[
			Tree.Leaf "#edges:";
			Tree.Node [STD.int (List.length edges)];
			Tree.Leaf "TACX:";
			Tree.Node [Nni.TACX.dump_stats tacx]
		]
	)
	| Cpx ->
	(
		let tacx, edges = Cpx.TACX.stree_load stree in
		Tree.Node[
			Tree.Leaf "#edges:";
			Tree.Node [STD.int (List.length edges)];
			Tree.Leaf "TACX:";
			Tree.Node [Cpx.TACX.dump_stats tacx]
		]
	) in
	STree.dumpfile [stree] fileB
