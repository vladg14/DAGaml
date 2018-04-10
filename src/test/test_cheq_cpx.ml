module T = Cpx;;

let fileX = Sys.argv.(1) in
let fileY = Sys.argv.(2) in

let file_out = Sys.argv.(3) in

let pure = T.newman () in
let _, edgesX = T.GroBdd.loadfile ~man:pure fileX in
let _, edgesY = T.GroBdd.loadfile ~man:pure fileY in

assert(List.length edgesX = List.length List.length edgesY);
List.iter2 (fun x y -> assert(x = y)) edgesX edgesY;

exit 0;
