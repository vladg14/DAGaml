assert(Array.length Sys.argv > 2);
(* TODO: display --help *)

let file = Sys.argv.(1) in
let pure_cp, edges = Cp.GroBdd.loadfile file in
let pure_zdd = Zdd.GroBdd.newman () in
let evaman, calc = Cp.PURE_TO_ZDD.newman pure_cp pure_zdd in

let edges = List.map calc edges in
STree.pprint [Cp.PURE_TO_ZDD.dump_stats evaman];

let file = Sys.argv.(2) in
Zdd.GroBdd.dumpfile pure_zdd edges file;

if(Array.length Sys.argv > 3)
then
(
    let strman = Udag.String.newman() in
    let stredges = Zdd.GroBdd.to_dot pure_zdd strman edges in
    Udag.String.to_dot_file strman stredges Sys.argv.(3)
);

exit 0;
