let conv_stree_stree stree args =
	let tacx_cp, edges = Cp.TACX.stree_load stree in
	let evaman, (tacx_cpB, edges) = CpB.TACX.cp_to_cpb tacx_cp edges in
	ConvArgsTypes.map_stats (fun()->CpB.TACX.CP_TO_CPB.dump_stats evaman) args;
	CpB.TACX.stree_dump tacx_cpB edges
