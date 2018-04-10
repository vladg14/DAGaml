let conv_cp_to_nni streeA args =
	let tacx_cp, edges = Cp.TACX.stree_load streeA in
	let tacx_nni = Nni.TACX.newman () in
	let evaman, mapcalc = Nni.TACX_OF_CP.newman tacx_cp tacx_nni in
	let edges = mapcalc edges in
	ConvArgsTypes.map_stats (fun()->Nni.TACX_OF_CP.dump_stats evaman) args;
	Nni.TACX.stree_dump tacx_nni edges

let conv_cp_to_cpx streeA args =
	let tacx_cp, edges = Cp.TACX.stree_load streeA in
	let tacx_nni = Cpx.TACX.newman () in
	let evaman, mapcalc = Cpx.TACX_OF_CP.newman tacx_cp tacx_nni in
	let edges = mapcalc edges in
	ConvArgsTypes.map_stats (fun()->Cpx.TACX_OF_CP.dump_stats evaman) args;
	Cpx.TACX.stree_dump tacx_nni edges

open ConvTypes

let conv_stree_stree modele stree args = match modele with
| Bryant	-> failwith "no conversion available from .cp.tacx to .bryant.tacx"
| Zdd			-> failwith "no conversion available from .cp.tacx to .zdd.tacx"
| Cp      -> stree
| Nni     -> conv_cp_to_nni stree args
| Cpx			-> conv_cp_to_cpx stree args
