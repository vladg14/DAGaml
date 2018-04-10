assert(Array.length Sys.argv > 2);
(* TODO: display --help *)

let file = Sys.argv.(2) in
match Sys.argv.(1) with
| "--cp" ->
(
let tacx, edges = Cp.TACX.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
STree.pprint [Cp.TACX.dump_stats tacx]
)
| "--cpx" ->
(
let tacx, edges = Cpx.TACX.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
STree.pprint [Cpx.TACX.dump_stats tacx]
)
| "--nni" ->
(
let tacx, edges = Nni.TACX.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
STree.pprint [Nni.TACX.dump_stats tacx]
)
| "--bryant" -> assert false
(*
let tacx, edges = Bryant.TACX.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
STree.pprint [Bryant.TACX.dump_stats tacx]
*)
| "--zdd" -> assert false
(*
let tacx, edges = Zdd.TACX.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_string";"; print_newline();
STree.pprint [Zdd.TACX.dump_stats tacx]
*)
| _ -> assert false;;

exit 0;
