module T = NniB

let conv_stree_stree stree args =
	let tacx, edges = T.TACX.stree_load stree in
	let evaman, (pure, edges) = T.TACX.compile tacx edges in
	let stats () = Tree.Node [
			Tree.Leaf "TACX:";
			Tree.Node [ T.TACX.G1.dump_stats tacx ];
			Tree.Leaf "TOTAL:";
			Tree.Node [ T.GroBdd.G1.dump_stats pure.T.GroBdd.cons ];
			Tree.Leaf "COMPILE:";
			T.TACX.COMPILE.dump_stats evaman
	] in
	ConvArgsTypes.map_stats stats args;
	T.GroBdd.stree_dump pure.T.GroBdd.cons edges
