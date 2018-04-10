assert(Array.length Sys.argv > 2);
(* TODO: display --help *)

let file = Sys.argv.(1) in
let tacx_cp, edges = Cp.TACX.loadfile file in
let tacx_cpx = Cpx.TACX.newman () in
let evaman, mapcalc = Cpx.TACX_OF_CP.newman tacx_cp tacx_cpx in

let edges = mapcalc edges in
STree.pprint [Cpx.TACX_OF_CP.dump_stats evaman];

let file = Sys.argv.(2) in
Cpx.TACX.dumpfile tacx_cpx edges file;

if(Array.length Sys.argv > 3)
then
(
    let strman = Udag.String.newman() in
    let stredges = Cpx.TACX.to_dot tacx_cpx strman edges in
    Udag.String.to_dot_file strman stredges Sys.argv.(3)
);

exit 0;
