open IterExtra

let n = (int_of_string Sys.argv.(1));;

let file = Sys.argv.(2);;
let err_file = Sys.argv.(3);;

module T = Cpx.GroBdd;;

let man = T.newman ();;

let ( *! ) = T.push man;;

let and_man, ( &! ) = Cpx.AND.newman man;;
let xor_man, ( ^! ) = Cpx.XOR.newman man;;

let ande_man, ( &@ ) = Cpx.ANDE.newman man;;
let xore_man, ( ^@ ) = Cpx.XORE.newman man;;

let make_bool b = Cpx.make_const b 0;;

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
Cpx.GroBdd.dumpfile man (gn |> Iter.enumerate 0 $$ (count "T:1.0 : " 10000 (fun x -> x)) |> Iter.to_list) file;;
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
print_string "TEST 3.0 : xor primitives are consistent"; print_newline();;

Iter.iter (fun (x, y) -> assert((make_bool x) ^! (make_bool y) = (make_bool (x<>y)));) (Iter.gen_bool $* Iter.gen_bool);;
print_string "TEST 3.1 : xor is consistant"; print_newline();;
gn22 |> Iter.enumerate 0 $$ (count "T:2.0 : " 10000 (fun ((x0, x1), (y0, y1)) ->
	let x01 = x0 *! x1
	and y01 = y0 *! y1
	and xy0 = x0 ^! y0
	and xy1 = x1 ^! y1 in
	let calcX = x01 ^! y01
	and calcY = xy0 *! xy1 in
	if (calcX <> calcY)
	then
	(
		let edges = [x0; x1; y0; y1; x01; y01; xy0; xy1; calcX; calcY] in
		(*let strman = Udag.String.newman () in
		let stredges = Cpx.GroBdd.to_dot man strman edges in
		Udag.String.to_dot_file strman stredges err_file;*)
		ignore(Cpx.GroBdd.dumpfile man edges err_file);
		assert false;
	);
	)) |> Iter.iter ignore;;
print_newline();;

print_string "TEST 4.0 : partial evaluation is consistant"; print_newline();;

let gen_assign n = (Iter.of_list [None; Some false; Some true]) $^ n;;

let peman, peval = Cpx.PartEval.newman man;;

gn2 $* (gen_assign n) |> Iter.enumerate 0 $$ (count "T:2.0 : " 10000 (fun ((x0, x1), set) ->
	let x01 = x0 *! x1 in
	let pe_set_x0 = peval (Some set) x0 in
	let pe_set_x1 = peval (Some set) x1 in
	let pe_0set_x01 = peval (Some((Some false)::set)) x01 in
	let pe_1set_x01 = peval (Some((Some true )::set)) x01 in
	let pe_Nset_x01 = peval (Some( None       ::set)) x01 in
	let pe_set_x0_pe_set_x1 = pe_set_x0 *! pe_set_x1 in
	assert(pe_set_x0 = pe_0set_x01);
	assert(pe_set_x1 = pe_1set_x01);
	assert(pe_Nset_x01 = pe_set_x0_pe_set_x1);
	)) |> Iter.iter ignore;;
print_newline();;

print_string "TEST 5.0 : ande primitives are consistent"; print_newline();;

Iter.iter (fun (x, y) -> assert((make_bool x) &@ (make_bool y) = (make_bool (x&&y)));) (Iter.gen_bool $* Iter.gen_bool);;
print_string "TEST 5.1 : ande is consistant"; print_newline();;
gn22 |> Iter.enumerate 0 $$ (count "T:2.0 : " 10000 (fun ((x0, x1), (y0, y1)) ->
	let x01 = x0 *! x1
	and y01 = y0 *! y1
	and xy0 = x0 &@ y0
	and xy1 = x1 &@ y1 in
	let calcX = x01 &@ y01
	and calcY = xy0 *! xy1 in
	assert(calcX = calcY);
	)) |> Iter.iter ignore;;
print_newline();;
print_string "TEST 6.0 : xore primitives are consistent"; print_newline();;

Iter.iter (fun (x, y) -> assert((make_bool x) ^! (make_bool y) = (make_bool (x<>y)));) (Iter.gen_bool $* Iter.gen_bool);;
print_string "TEST 6.1 : xore is consistant"; print_newline();;
gn22 |> Iter.enumerate 0 $$ (count "T:2.0 : " 10000 (fun ((x0, x1), (y0, y1)) ->
	let x01 = x0 *! x1
	and y01 = y0 *! y1
	and xy0 = x0 ^@ y0
	and xy1 = x1 ^@ y1 in
	let calcX = x01 ^@ y01
	and calcY = xy0 *! xy1 in
	if (calcX <> calcY)
	then
	(
		let edges = [x0; x1; y0; y1; x01; y01; xy0; xy1; calcX; calcY] in
		(*let strman = Udag.String.newman () in
		let stredges = Cpx.GroBdd.to_dot man strman edges in
		Udag.String.to_dot_file strman stredges err_file;*)
		ignore(Cpx.GroBdd.dumpfile man edges err_file);
		assert false;
	);
	)) |> Iter.iter ignore;;
print_newline();;
