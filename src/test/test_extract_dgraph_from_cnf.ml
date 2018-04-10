open IterExtra

let file_in  = Sys.argv.(1)
and file_out = Sys.argv.(2);;

open StrLoadCnf
open IoUtils

let my_cnf = load (Stream.of_channel (open_in file_in));;

let graph = GraphAA.make my_cnf.CnfTypes.input false;;

let eval_closure liste =
	let iter = Iter.of_list liste in
	Iter.iter (fun ((_, x), (_, y)) -> GraphAA.set graph (x, y)) (iter $* iter);
	()
;;

List.iter eval_closure my_cnf.CnfTypes.clauses;;
(*
print_string (Matrix.pretty_bool_matrix graph.GraphAA.edges);;
print_newline();;
*)
STree.dumpfile [GraphLA.strdump_graph (GraphAA.la_of_aa graph)] file_out;;

exit 0;;
