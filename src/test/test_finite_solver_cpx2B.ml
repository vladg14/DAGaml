assert(Array.length Sys.argv > 2);
(* TODO: display --help *)

let file_in  = Sys.argv.(1) in
let file_out = Sys.argv.(2) in

print_string "load tacx : start"; print_newline();
let tacx, edges = Cpx2B.TACX.loadfile file_in in
print_string "load tacx : done "; print_newline();
let pman, edges = Cpx2B.TACX.finite_solver tacx edges in


STree.pprint [Cpx2B.TACX.FINITE_SOLVER.dump_stats pman];

Cpx2B.TACX.dumpfile tacx edges file_out;

if(Array.length Sys.argv > 3)
then(Cpx2B.TACX.dotfile tacx edges Sys.argv.(3));;

exit 0;
