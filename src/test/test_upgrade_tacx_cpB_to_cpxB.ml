module SRC = CpB.TACX;;
module DST = Cpx2B.TACX;;

assert(Array.length Sys.argv > 2);
(* TODO: display --help *)



let file_in  = Sys.argv.(1)
and file_out = Sys.argv.(2) in
let tacx_cp, edges = SRC.loadfile file_in in
let evaman, (tacx_cpx, edges) = DST.of_cp tacx_cp edges in

STree.pprint [DST.OF_CP.dump_stats evaman];

DST.dumpfile tacx_cpx edges file_out;

if(Array.length Sys.argv > 3)
then (DST.dotfile tacx_cpx edges Sys.argv.(3));;

exit 0;
