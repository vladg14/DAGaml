assert(Array.length Sys.argv > 1);
(* TODO: display --help *)

let file = Sys.argv.(2) in
match Sys.argv.(1) with
| "--cp" ->
(
let pure, edges = Cp.GroBdd.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_newline();
let getsize, apply = Cp.GetSize.newman pure in
List.iter apply edges;
print_string "size = "; print_int (Cp.GetSize.get getsize); print_newline();
STree.pprint [Cp.GroBdd.dump_stats pure]
)
| "--cpx" ->
(
let pure, edges = Cpx.GroBdd.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_newline();
let getsize, apply = Cpx.GetSize.newman pure in
List.iter apply edges;
print_string "size = "; print_int (Cpx.GetSize.get getsize); print_newline();
STree.pprint [Cpx.GroBdd.dump_stats pure]
)
| "--nni" ->
(
let pure, edges = Nni.GroBdd.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_newline();
let getsize, apply = Nni.GetSize.newman pure in
List.iter apply edges;
print_string "size = "; print_int (Nni.GetSize.get getsize); print_newline();
STree.pprint [Nni.GroBdd.dump_stats pure]
)
| "--bryant" ->
(
let pure, edges = Bryant.GroBdd.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_newline();
let getsize, apply = Bryant.GetSize.newman pure in
List.iter apply edges;
print_string "size = "; print_int (Bryant.GetSize.get getsize); print_newline();
STree.pprint [Bryant.GroBdd.dump_stats pure]
)
| "--zdd" ->
(
let pure, edges = Zdd.GroBdd.loadfile file in
print_string "#edges = "; print_int (List.length edges); print_newline();
let getsize, apply = Zdd.GetSize.newman pure in
List.iter apply edges;
print_string "size = "; print_int (Zdd.GetSize.get getsize); print_newline();
STree.pprint [Zdd.GroBdd.dump_stats pure]
)
| _ -> assert false;;

exit 0;
