module T = Cpx2B;;

assert(Array.length Sys.argv >= 2);

let file_in  = Sys.argv.(1) in

let tacx, edges = T.TACX.loadfile file_in in

print_string "tacx system: ";
print_string (if T.TACX.check tacx edges
then "checked"
else "error");
print_newline();


