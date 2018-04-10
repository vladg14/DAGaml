open IterExtra

let n = (int_of_string Sys.argv.(1));;

let file = Sys.argv.(2);;

module T = Nni.GroBdd;;

let man = T.newman ();;

let ( *! ) = T.push man;;

let and_man, ( &! ) = Nni.AND.newman man;;
let xor_man, ( ^! ) = Nni.XOR.newman man;;

let make_bool b = Nni.make_const b 0;;

let gen_bnb =
	let gen_bool = Iter.gen_bool $$ make_bool in
	let rec aux (lx: T.edge Iter.iter) = function
		| 0 ->	lx
		| n ->	( aux (lx $* lx $$ (fun (x, y) -> x *! y)) (n-1) )
	in (fun n -> assert(n>=0); aux gen_bool n)
;;


let count s n f (i, x) =
	if i mod n = 0 then (print_string s; print_int i; print_string "\r"; flush stdout);
	f x
;;

let gn = gen_bnb n;;
let gn2 = gn $* gn;;
let gn22 = gn2 $* gn2;;

print_string "TEST 1.0 : cons is reversible"; print_newline();;
Nni.GroBdd.dumpfile man (gn |> Iter.enumerate 0 $$ (count "T:1.0 : " 10000 (fun x -> x)) |> Iter.to_list) file;;
print_newline();;


print_string "TEST 2.0 : and primitives are consistent"; print_newline();;

Iter.iter (fun (x, y) -> assert((make_bool x) &! (make_bool y) = (make_bool (x&&y)));) (Iter.gen_bool $* Iter.gen_bool);;

print_string "TEST 2.1 : and is consistant"; print_newline();;
gn22 |> Iter.enumerate 0 $$ (count "T:2.0 : " 10000 (fun ((x0, x1), (y0, y1)) ->
	let x01 = x0 *! x1
	and y01 = y0 *! y1
	and xy0 = x0 &! y0
	and xy1 = x1 &! y1 in
	let calcX = x01 &! y01
	and calcY = xy0 *! xy1 in
	assert(calcX = calcY);
	)) |> Iter.iter ignore;;
print_newline();;

print_string "TEST 3.0 : xor is consistant"; print_newline();;
gn22 |> Iter.enumerate 0 $$ (count "T:2.0 : " 10000 (fun ((x0, x1), (y0, y1)) ->
	assert(((x0 *! x1) ^! (y0 *! y1)) = ((x0 ^! y0) *! (x1 ^! y1)));
	)) |> Iter.iter ignore;;
print_newline();;
