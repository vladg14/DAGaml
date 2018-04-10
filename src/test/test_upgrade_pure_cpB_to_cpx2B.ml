module SRC = CpB.GroBdd;;
module DST = Cpx2B.GroBdd;;

assert(Array.length Sys.argv > 2);;

let file_in  = Sys.argv.(1);;
let file_out = Sys.argv.(2);;
let man_cp, edges = SRC.loadfile file_in;;
let evaman, (man_cpx, edges) = DST.of_cp man_cp edges;;

STree.pprint [DST.OF_CP.dump_stats evaman];;
DST.dumpfile man_cpx edges file_out;;

if(Array.length Sys.argv > 3)
then (DST.dotfile man_cpx edges Sys.argv.(3));;

let cntman, cnt_edges = DST.cntsat man_cpx edges ;;
print_string "CntSat  = [";;
List.iter (fun cnt -> BigInt.print cnt; print_string "; ") cnt_edges;
print_string "];";;
print_newline();;
let cntman, cnt_edges = DST.cntsat' man_cpx edges ;;
print_string "CntSat' = [";
List.iter (fun cnt -> BigInt.print cnt; print_string "; ") cnt_edges;;
print_string "];";;
print_newline();;

exit 0;;
