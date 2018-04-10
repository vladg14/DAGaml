assert(Array.length Sys.argv > 2);
(* TODO: display --help *)

let file_in  = Sys.argv.(1)
and file_out = Sys.argv.(2) in
let tacx_cp, edges = Cp.TACX.loadfile file_in in
let evaman, (tacx_cpB, edges) = CpB.TACX.cp_to_cpb tacx_cp edges in

STree.pprint [CpB.TACX.CP_TO_CPB.dump_stats evaman];

CpB.TACX.dumpfile tacx_cpB edges file_out;

if(Array.length Sys.argv > 3)
then (CpB.TACX.dotfile tacx_cpB edges Sys.argv.(3));;

exit 0;;
