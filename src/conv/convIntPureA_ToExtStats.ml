open ConvTypes
let conv_stree_file modele stree fileB args =
	let stree = match modele with
	| Bryant -> failwith "WIP export .bryant.tacx to stats"
	(*
	let tacx, edges = Bryant.GroBdd.stree_load stree in
	print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
	STree.pprint [Bryant.GroBdd.dump_stats tacx]
	*)
	| Zdd -> failwith "WIP export .zdd.tacx to stats"
	(*
	let tacx, edges = Zdd.GroBdd.stree_load stree in
	print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
	STree.pprint [Zdd.GroBdd.dump_stats tacx]
	*)
	| Cp ->
	(
		let tacx, edges = Cp.GroBdd.stree_load stree in
		Tree.Node[
			Tree.Leaf "#edges:";
			Tree.Node [STD.int (List.length edges)];
			Tree.Leaf "GroBdd:";
			Tree.Node [Cp.GroBdd.dump_stats tacx]
		]
	)
	| Nni ->
	(
		let tacx, edges = Nni.GroBdd.stree_load stree in
		Tree.Node[
			Tree.Leaf "#edges:";
			Tree.Node [STD.int (List.length edges)];
			Tree.Leaf "GroBdd:";
			Tree.Node [Nni.GroBdd.dump_stats tacx]
		]
	)
	| Cpx ->
	(
		let tacx, edges = Cpx.GroBdd.stree_load stree in
		Tree.Node[
			Tree.Leaf "#edges:";
			Tree.Node [STD.int (List.length edges)];
			Tree.Leaf "GroBdd:";
			Tree.Node [Cpx.GroBdd.dump_stats tacx]
		]
	) in
	STree.dumpfile [stree] fileB
