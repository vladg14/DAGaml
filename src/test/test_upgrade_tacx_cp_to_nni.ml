assert(Array.length Sys.argv > 2);
(* TODO: display --help *)

let file = Sys.argv.(1) in
let tacx_cp, edges = Cp.TACX.loadfile file in
let tacx_nni = Nni.TACX.newman () in
let evaman, mapcalc = Nni.TACX_OF_CP.newman tacx_cp tacx_nni in

let edges = mapcalc edges in
STree.pprint [Nni.TACX_OF_CP.dump_stats evaman];

let file = Sys.argv.(2) in
Nni.TACX.dumpfile tacx_nni edges file;

if(Array.length Sys.argv > 3)
then
(
    let strman = Udag.String.newman() in
    let stredges = Nni.TACX.to_dot tacx_nni strman edges in
    Udag.String.to_dot_file strman stredges Sys.argv.(3)
);

exit 0;
