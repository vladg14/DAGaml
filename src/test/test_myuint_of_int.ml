let start = Sys.time() ;;

let n = (int_of_string Sys.argv.(1));;

print_string "------ begin computation ------";;
print_newline();;

module T = Cp.TACX;;

let man = T.newman ();;

let ( *! ) : T.edge -> T.edge -> T.edge
	= Cp.( *! ) man
and ( &! ) : T.edge -> T.edge -> T.edge
	= Cp.( &! ) man
and ( ^! ) : T.edge -> T.edge -> T.edge
	= Cp.( ^! ) man;;

let no = Cp.no;;

let addi : MyUint.t -> int -> MyUint.t = MyUint.addi man;;
let eq : MyUint.t -> MyUint.t -> T.edge = MyUint.eq man;;

let x, c = MyUint.range man n;;

print_string "------ end of computation ------";;
print_newline();;

print_string "time:\t";;
print_float (Sys.time() -. start);;
print_string " s.";;
print_newline();;


let name = "workdir/"^(string_of_int n)^"-test01.cp.tacx";;
print_string name; print_newline();;
let edges = [c];;

let dump_man = Udag.STree.newman ();;
let dump_edges = T.dump man dump_man edges;;
let dump_edges = Udag.STree.dump dump_man dump_edges;;
STree.dumpfile [dump_edges] name;;

let strman = Udag.String.newman ();;
let stredges = T.to_dot man strman edges;;
Udag.String.to_dot_file strman stredges (name^".dot");;

exit 0;;
