module T = Cp.TACX;;

let man = T.newman();;

let ( *! ) = Cp.( *! ) man
and ( &! ) = Cp.( &! ) man
and ( ^! ) = Cp.( ^! ) man;;

let make_const  b n = Cp.TACX.push_leaf (b, MyList.ntimes CpTypes.P n) ();;

let make_ident b n = (make_const b n) *! (make_const (not b) n);;

let push_pass = Cp.push_pass;;

let ( =?? ) = Cp.( =?? );;

let no = Cp.no;;

let c0 = make_const false 0;;
let c1 = make_const true 0;;
let x1 = make_ident false 0;; (* function x -> x *)
let nx1 = make_ident true 0;; (* function x -> -x *)
let x01 = x1 *! x1;; (* function x y -> y *)
let nx01 = nx1 *! nx1;; (* function x y -> -y *)
let x10 = make_ident false 1;;
let nx10 = make_ident true 1;;


let (=!!) = Cp.( =?? );;

assert(c0 &! c0 =!! c0);;
assert(c0 &! c1 =!! c0);;
assert(c1 &! c0 =!! c0);;
assert(c1 &! c1 =!! c1);;

assert(c0 ^! c0 =!! c0);;
assert(c0 ^! c1 =!! c1);;
assert(c1 ^! c0 =!! c1);;
assert(c1 ^! c1 =!! c0);;

assert(nx01 ^! nx10 =!! x01 ^! x10);;

let dump_man = Udag.STree.newman ();;

let edges = [nx01; nx10; nx01 ^! nx10; x01; x10; x01 ^! x10];;


let strman = Udag.String.newman ();;
let stredges = Cp.TACX.to_dot man strman edges;;

let stredges = Udag.String.to_dot_file strman stredges "test.dot";;

let dump_edges = T.dump man dump_man edges;;

let dump_edges = Udag.STree.dump dump_man dump_edges;;

STree.pprint [dump_edges];;
STree.dumpfile [dump_edges] "test.cp.tacx";;

let groman = Cp.newman ();;

let evaman, mapcalc = Cp.EVAL.newman man groman;;

let eval_edges = mapcalc edges;;

let dump_edges = Cp.GroBdd.dump groman dump_man eval_edges;;
let dump_edges = Udag.STree.dump dump_man dump_edges;;

STree.pprint [dump_edges];;
STree.dumpfile [dump_edges] "test.cp.pure";;

(*STree.pprint [AND.dump_stats and_man; XOR.dump_stats xor_man];;*)

