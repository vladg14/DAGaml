assert(Array.length Sys.argv > 2);
(* TODO: display --help *)

let file = Sys.argv.(1) in
let pure_cp, edges = Cp.GroBdd.loadfile file in
let pure_bryant = Bryant.GroBdd.newman () in
let evaman, calc = Cp.PURE_TO_BRYANT.newman pure_cp pure_bryant in

let edges = List.map calc edges in
STree.pprint [Cp.PURE_TO_BRYANT.dump_stats evaman];

let file = Sys.argv.(2) in
Bryant.GroBdd.dumpfile pure_bryant edges file;

if(Array.length Sys.argv > 3)
then
(
    let strman = Udag.String.newman() in
    let stredges = Bryant.GroBdd.to_dot pure_bryant strman edges in
    Udag.String.to_dot_file strman stredges Sys.argv.(3)
);

exit 0;
