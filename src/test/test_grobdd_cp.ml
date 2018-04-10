let man = Cp.newman();;

let make_const = Cp.make_const;;

let make_ident = Cp.make_ident man;;

let push_pass = Cp.push_pass;;

let ( *? ) = Cp.GroBdd.push man;;

let ( =?? ) = Cp.( =?? );;

let no = Cp.no;;

let c0 = make_const false 0;;
let c1 = make_const true 0;;
let x1 = make_ident false 0;; (* function x -> x *)
let nx1 = make_ident true 0;; (* function x -> -x *)
let x01 = x1 *? x1;; (* function x y -> y *)
let nx01 = nx1 *? nx1;; (* function x y -> -y *)
let x10 = make_ident false 1;;
let nx10 = make_ident true 1;;


let and_man, (&?) = Cp.AND.newman man;;
let xor_man, (^?) = Cp.XOR.newman man;;

assert(c0 &? c0 =?? c0);
assert(c0 &? c1 =?? c0);
assert(c1 &? c0 =?? c0);
assert(c1 &? c1 =?? c1);

assert(c0 ^? c0 =?? c0);
assert(c0 ^? c1 =?? c1);
assert(c1 ^? c0 =?? c1);
assert(c1 ^? c1 =?? c0);

(*
let c0 = T.push_leaf (false, []) ();;
let c1 = T.push_leaf (true , []) ();;
let x0 = T.push man c0 c1;;
let nx0 = T.push man c1 c0;;
let x1 = T.push man x0 x0;;
let nx1 = T.push man nx0 nx0;;
*)

assert(nx01 ^? nx10 =?? x01 ^? x10);;

STree.pprint [Cp.AND.dump_stats and_man; Cp.XOR.dump_stats xor_man];;

let dump_man = Udag.STree.newman ();;
let strman = Udag.String.newman ();;

let edges0 = [nx01; nx10; nx01 ^? nx10; x01; x10; x01 ^? x10];;

Cp.GroBdd.dumpfile man edges0 "test.cp.pure";;

let man1, edges1 = Cp.GroBdd.loadfile "test.cp.pure";;



exit 0;;
