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

let (==) : MyUint.t -> MyUint.t -> T.edge = MyUint.eq man;;
let (++) : MyUint.t -> MyUint.t -> MyUint.t = MyUint.add man;;

let v = MyUint.copy_t 2 (MyUint.input man n);;
let x = v.(0)
and y = v.(1);;

let s = x ++ y;;

print_string "------ end of computation ------";;
print_newline();;

print_string "time:\t";;
print_float (Sys.time() -. start);;
print_string " s.";;
print_newline();;


let name = "workdir/"^(string_of_int n)^"-add.cp.tacx";;
let edges = (Array.to_list (MyUint.get_array s));;

let dump_man = Udag.STree.newman ();;
let dump_queens = T.dump man dump_man edges;;
let dump_queens = Udag.STree.dump dump_man dump_queens;;
STree.dumpfile [dump_queens] name;;

let strman = Udag.String.newman ();;
let strqueens = T.to_dot man strman edges;;
Udag.String.to_dot_file strman strqueens (name^".dot");;

exit 0;;
