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

let x, c = MyUint.range_r man n;;
let c = Oops.vect_et (&!) (Oops.copy_fun_t n c);;
let xx = Array.init n (addi x);;
let xxy = Array.map (MyUint.copy_t n) xx;;
let xy = MyUint.copy_t n x;;


let matD = Array.init n (fun x -> Array.init x (fun y ->
	assert(y < x);
	let h = x - y in
	let a = Cp.no (eq xxy.(h).(x) xxy.(0).(y))
	and b = Cp.no (eq xxy.(h).(y) xxy.(0).(x)) in
	(a &! b)
));;

let diags = Oops.list_et (&!) (List.flatten (List.map Array.to_list (Array.to_list matD)));;

let matL = Array.init n (fun x -> Array.init x (fun y ->
	assert(y < x);
	Cp.no (eq xy.(x) xy.(y))
));;

let lines = Oops.list_et (&!) (List.flatten (List.map Array.to_list (Array.to_list matL)));;

let n_queens = c &! lines &! diags ;; 


print_string "------ end of computation ------";;
print_newline();;

print_string "time:\t";;
print_float (Sys.time() -. start);;
print_string " s.";;
print_newline();;


let name = "workdir/"^(string_of_int n)^"-rtcqueens.cp.tacx"

let dump_man = Udag.STree.newman ();;
let dump_queens = T.dump man dump_man [n_queens];;
let dump_queens = Udag.STree.dump dump_man dump_queens;;
STree.dumpfile [dump_queens] name;;

let strman = Udag.String.newman ();;
let strqueens = T.to_dot man strman [n_queens];;
Udag.String.to_dot_file strman strqueens (name^".dot");;

exit 0;;
