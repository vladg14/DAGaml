module T = CpB

let conv_stree_stree stree args =
	let tacx, edges = T.TACX.stree_load stree in
	let pure = T.newman () in
	let evaman = T.TACX.COMPILE.newman tacx pure in
	let edges = List.map (T.TACX.COMPILE.rec_edge evaman) edges in
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
