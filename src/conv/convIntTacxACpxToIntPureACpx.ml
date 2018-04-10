module T = Cpx

let conv_stree_stree stree args =
	let tacx, edges = T.TACX.stree_load stree in
	let pure = T.newman () in
	let evaman, mapcalc = T.EVAL.newman tacx pure in

	let edges = mapcalc edges in
	let stats() = Tree.Node [
			Tree.Leaf "TACX:";
			Tree.Node [ T.TACX.dump_stats tacx ];
			Tree.Leaf "TOTAL:";
			Tree.Node [ T.GroBdd.dump_stats pure ];
			Tree.Leaf "EVAL:";
			Tree.Node [ T.EVAL.dump_stats evaman ];
	] in
	ConvArgsTypes.map_stats stats args;

	T.GroBdd.stree_dump pure edges
