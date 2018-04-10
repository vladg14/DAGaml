module T = Cpx2B;;

let file = Sys.argv.(1) in
let tacx, edges = T.TACX.loadfile file in

print_string "start computation"; print_newline();
let evaman, (pure, edges) = T.TACX.compile tacx edges in
print_string "end of computation"; print_newline();
STree.pprint [
	Tree.Node [
		Tree.Leaf "TACX:";
		Tree.Node [ T.TACX.G1.dump_stats tacx ];
		Tree.Leaf "TOTAL:";
		Tree.Node [ T.GroBdd.dump_stats pure ];
	];
	T.TACX.COMPILE.dump_stats evaman];

let cntman, cnt_edges = T.GroBdd.cntsat (T.GroBdd.get_cman pure) edges in
print_string "CntSat  = [";
List.iter (fun cnt -> BigInt.print cnt; print_string "; ") cnt_edges;
print_string "];";
print_newline();
let cntman, cnt_edges = T.GroBdd.cntsat' (T.GroBdd.get_cman pure) edges in
print_string "CntSat' = [";
List.iter (fun cnt -> BigInt.print cnt; print_string "; ") cnt_edges;
print_string "];";
print_newline();

let file = Sys.argv.(2) in
T.GroBdd.dumpfile (T.GroBdd.get_cman pure) edges file;

(*
STree.pprint [Tree.Node[Tree.Leaf "FINAL:"; Tree.Node[Udag.STree.dump_stats strman]]];
*)


exit 0;
