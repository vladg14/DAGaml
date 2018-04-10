
let conv_cp_to_nni stree args =
	let tacx_cp, edges = CpB.TACX.stree_load stree in
	let evaman, (tacx_cpx, edges) = NniB.TACX.of_cp tacx_cp edges in
	ConvArgsTypes.map_stats (fun()->NniB.TACX.OF_CP.dump_stats evaman) args;
	NniB.TACX.stree_dump tacx_cpx edges

let conv_cp_to_cpx stree args =
	let tacx_cp, edges = CpB.TACX.stree_load stree in
	let evaman, (tacx_cpx, edges) = Cpx2B.TACX.of_cp tacx_cp edges in
	ConvArgsTypes.map_stats (fun()->Cpx2B.TACX.OF_CP.dump_stats evaman) args;
	Cpx2B.TACX.stree_dump tacx_cpx edges

open ConvTypes

let conv_stree_stree modele stree args = match modele with
| Bryant	-> failwith "no conversion available from .cpB.tacx to .bryantB.tacx"
| Zdd			-> failwith "no conversion available from .cpB.tacx to .zddB.tacx"
| Cp      -> stree
| Nni     -> conv_cp_to_nni stree args
| Cpx			-> conv_cp_to_cpx stree args

(*
module SRC = CpB.TACX
module DST = NniB.TACX

let conv_stree_stree stree args =
	let tacx_cp, edges = SRC.stree_load file_in in
	let evaman, (tacx_cpx, edges) = DST.of_cp tacx_cp edges in
	ConvArgsTypes.map_stats (fun()->DST.OF_CP.dump_stats evaman) args;
	DST.stree_dump tacx_cpx edges
*)
