module SRC = CpB.GroBdd
module DST = Cpx2B.GroBdd

let conv_stree_stree stree args =
	let man_cp, edges = SRC.stree_load stree in
	let evaman, (man_cpx, edges) = DST.of_cp man_cp edges in
	ConvArgsTypes.map_stats (fun()->DST.OF_CP.dump_stats evaman) args;
	DST.stree_dump man_cpx edges
