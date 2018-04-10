module T = Cp;;

let file = Sys.argv.(1) in
let tacx, edges = T.TACX.loadfile file in
let pure = T.newman () in
let evaman, mapcalc = T.EVAL.newman tacx pure in

let edges = mapcalc edges in
STree.pprint [
	Tree.Node [
		Tree.Leaf "TACX:";
		Tree.Node [ T.TACX.dump_stats tacx ];
		Tree.Leaf "TOTAL:";
		Tree.Node [ T.GroBdd.dump_stats pure ];
	];
	T.EVAL.dump_stats evaman];

let cntman, cntsat = T.CntSat.newman pure in 
print_string "CntSat = [";
List.iter Extra.(cntsat >> (fun x -> BigInt.print x; print_string "; ")) edges;
print_string "];";
print_newline();

let file = Sys.argv.(2) in
let strman = T.GroBdd.dumpfile pure edges file in

STree.pprint [Tree.Node[Tree.Leaf "FINAL:"; Tree.Node[Udag.STree.dump_stats strman]]];


exit 0;
