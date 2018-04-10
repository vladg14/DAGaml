let conv_stree_stree stree args =
	let pure_cp, edges = Cp.GroBdd.stree_load stree in
	let pure_bryant = Bryant.GroBdd.newman () in
	let evaman, calc = Cp.PURE_TO_BRYANT.newman pure_cp pure_bryant in

	let edges = List.map calc edges in
	ConvArgsTypes.map_stats (fun()->Cp.PURE_TO_BRYANT.dump_stats evaman) args;

	Bryant.GroBdd.stree_dump pure_bryant edges
