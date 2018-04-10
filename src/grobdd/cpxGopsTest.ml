(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open IterExtra
open CpxTypes
open CpxUtils
open CpxDumpLoad
open CpxGops


let check_contig m =
	let n = List.length m in
	let t = Array.make n 0 in
	List.iter (fun x -> t.(x) <- t.(x)+1) m;
	let rec aux0 = function
		| [] -> true
		| 0::tail -> aux0 tail
		| _::tail -> false
	in
	let rec aux1 = function
		| [] -> true
		| 0::tail -> aux0 tail
		| _::tail -> aux1 tail
	in
	aux1 (Array.to_list t)
;;

List.iter print_string (MyList.init 10 string_of_int); print_newline();;

let dummydump m = "["^(StrUtil.catmap ", " string_of_int m)^" ]";;

let gen1 n = (Iter.range 0 n) $^ n |> (Iter.filter check_contig);;
let gen_contig = gen1 ;;

let gen_vec n = Iter.gen_bool $^ n;;

let gen_sub' n = (Iter.range 0 3) $^ n $@++ (fun vect ->
	let n2 = MyList.count(function 2 -> true | _ -> false) vect in
	(gen_contig n2) $* (gen_vec n2) $$+ (fun (a, b) ->
		let rec f carry = function
			| ([], []) -> List.rev carry
			| (0::x, y) -> f (S::carry) (x, y)
			| (1::x, y) -> f (P::carry) (x, y)
			| (2::x, (i, b)::y) -> f(X(b, i)::carry) (x, y)
			| _ -> assert false
		in f [] (vect, List.combine a b)))
;;

let gen_sub n = gen_sub' n Iter.stop;;

let gen_block n = (Iter.gen_bool $* Iter.gen_bool $* gen_sub n) $$ (fun ((neg, shift), sub) -> {neg; shift; sub});;

let gen_block_checked n = gen_block n |> (Iter.filter check);;

let n = int_of_string(Sys.argv.(1));;

print_string "TEST 1.0 : contig"; print_newline();;
(*Iter.iter (fun x -> print_string (dummydump x); print_newline()) (gen1 n);*)
print_string "LENGTH = "; print_int (Iter.length (gen1 n)); print_newline();;
print_string "TEST 2.0 : block"; print_newline();;
(*Iter.iter (fun x -> print_string (block_dummydump x); print_newline()) (gen_block n);*)
print_string "LENGTH = "; print_int (Iter.length (gen_block n)); print_newline();;
print_string "TEST 2.1 : reduced block are correct"; print_newline();;
Iter.iter (fun x ->
	let y = reduce x in
	assert(check y)) (gen_block n);
print_string "TEST 2.2 : reduce^2 = reduce1"; print_newline();;
Iter.iter (fun x ->
	let y = reduce x in
	let z = reduce y in
	assert(y = z)) (gen_block n);
print_string "TEST 2.3 : reduce using in_block"; print_newline();;
Iter.iter (fun x ->
	let r = reduce x in
	if x <> r then
	(
		Iter.iter (fun vec ->
		assert(in_block x vec = in_block r vec);
		()
	)) (gen_vec n)) (gen_block n);
print_string "TEST 3.0 : checked block"; print_newline();;
(*Iter.iter (fun x -> print_string (block_to_pretty x); print_newline(); print_newline()) (gen_block_checked n);*)
print_string "LENGTH = "; print_int (Iter.length (gen_block_checked n)); print_newline();;
print_string "TEST 3.0.1 : reduce on correct block is identity"; print_newline();;
Iter.iter (fun x ->
	let y = reduce x in
	assert(x = y)) (gen_block_checked n);
print_string "TEST 3.0.2-1 : binDump/Load int"; print_newline();;
Iter.iter (fun x ->
	let y = BinDump.int x [] in
	let z, s = BinLoad.int y in
	assert(s = []);
	assert(z = x)) (Iter.range 0 256);
print_string "TEST 3.0.2 : binDump/Load checked block"; print_newline();;
Iter.iter (fun x ->
	let y = bindump_block x [] in
	let z, s = binload_block y in
	assert(s = []);
	assert(x = z)) (gen_block_checked n);
print_string "TEST 3.1 : solve_cons"; print_newline();;

let reversible ((ex, ix) as x) ((ey, iy) as y) =
	match solve_cons (fun x -> x) x y with
	| Utils.MEdge xy ->
	( match node_pull (fun x -> x) xy with
		| Utils.MEdge (x', y') -> assert((x = x')&&(y = y'))
		| Utils.MNode _ -> assert false
	)
	| Utils.MNode (e, (xy, ix', iy')) ->
	(
		(
			let xy' = bindump_block2 xy [] in
			let xy'', s = binload_block2 xy' in
			assert(s = []);
			assert(xy = xy'');
		);
		match node_pull (fun x -> x) (e, Utils.Node 3) with
			| Utils.MEdge _ -> assert false
			| Utils.MNode f -> let x', y' = f (xy, ix', iy') in
				assert(x = x' && y = y')
	);
	()
;;

Iter.iter (fun (ex, ey) ->
	let ix = if List.exists (function S -> true | _ -> false) ex.sub
		then Utils.Node 1
		else Utils.Leaf ()
	and iy, iy' = if List.exists (function S -> true | _ -> false) ey.sub
		then Utils.Node 1 , Utils.Node 2
		else Utils.Leaf (), Utils.Leaf ()
	in
	let x = (ex, ix)
	and y = (ey, iy)
	and y' = (ey, iy') in
	reversible x y;
	reversible x y')
	((gen_block_checked n) $* (gen_block_checked n));

print_string "TEST 3.2 : solve_cons using in_block"; print_newline();;

let compatible_test_3_2 ((ex, ix) as x) ((ey, iy) as y) =
	match solve_cons (fun x -> x) x y with
	| Utils.MEdge (exy, ixy) ->
	(
		Iter.iter (fun vec ->
			assert(in_block exy (false::vec) = in_block ex vec);
			assert(in_block exy (true ::vec) = in_block ey vec);
			()
		) (gen_vec n)
	)
	| Utils.MNode (e, (xy, ix', iy')) ->
	(
		let ok = function
			| None -> fun _ -> true
			| Some x -> function None -> false | Some y -> x = y
		in
		Iter.iter (fun vec ->
			assert(ok (in_block e (false::vec)) (in_block ex vec));
			assert(ok (in_block e (true ::vec)) (in_block ey vec));
			()
		) (gen_vec n)
	);
	()
;;

Iter.iter (fun (ex, ey) ->
	let ix = if List.exists (function S -> true | _ -> false) ex.sub
		then Utils.Node 1
		else Utils.Leaf ()
	and iy, iy' = if List.exists (function S -> true | _ -> false) ey.sub
		then Utils.Node 1 , Utils.Node 2
		else Utils.Leaf (), Utils.Leaf ()
	in
	let x = (ex, ix)
	and y = (ey, iy)
	and y' = (ey, iy') in
	compatible_test_3_2 x y;
	compatible_test_3_2 x y')
	((gen_block_checked n) $* (gen_block_checked n));

print_string "TEST 4.1 : check assign & compose compatibility"; print_newline();;

let gen_assign n = (Iter.of_list [None; Some false; Some true]) $^ n;;

Iter.iter (fun blockC ->
	let nS = count_nS blockC.sub in
	if nS > 0 then(
	Iter.iter (fun set ->
		let set = (Some set) in
		let (blockC', identC'), set' = assign set (blockC, Utils.Node 1) in
		match identC' with
		| Utils.Leaf () -> ()
		| Utils.Node x -> assert(x=1);
		(
			Iter.iter (fun blockc ->
				let identc = if count_nS blockc.sub = 0
					then (Utils.Leaf ())
					else (Utils.Node 0) in
				let (blockc', identc'), (set'' : bool option list option)= assign set' (blockc, identc) in
				let blockC'c' = compose blockC' (blockc', identc') in
				let blockCc = compose blockC (blockc, identc) in
				let blockCc', (set'3 : bool option list option) = assign set blockCc in
				assert(blockC'c' = blockCc' && set'' = set'3);
			) (gen_block_checked nS)
		)
	) (gen_assign n))
) (gen_block_checked n);;

print_string "TEST 4.2 : check assign using solve_cons"; print_newline();;
(*
let node_assign set blockC (block, ix, iy) =
	let (blockC', ident), set' = assign set (blockC, Utils.Node 3) in
	match ident with
	| Utils.Leaf () -> ; Utils.MEdge ((blockC', Utils.Leaf()), set')
	| Utils.Node node ->
	(
		let edge0, edge1 = node_split (block, ix, iy) in
		match set' with
		| None -> Utils.MEdge (blockC', Utils.Node node)
		| Some [] -> assert false
		| Some(head::tail) -> match head with
			| None ->
			(
				let edge0', set0' = assign tail edge0
				and edge1', set1' = assign tail edge1 in
				match solve_cons (fun x -> x) edge0' edge1' with
				| Utils.

			)
			| Some false ->
			(
				let edge0', set0' = assign tail edge0 in
			)
			| Some true  ->
			(
				let edge1', set1' = assign tail edge1 in
			)
	)
*)
let compatible_test_4_2 ((e0, i0) as x0) ((e1, i1) as x1) =
	match solve_cons (fun x -> x) x0 x1 with
	| Utils.MEdge ((e01, i01) as x01) ->
	(
		Iter.iter (fun set ->
			let pe_set_x0 = assign (Some set) x0
			and pe_set_x1 = assign (Some set) x1
			and pe_0set_x01 = assign (Some((Some false)::set)) x01
			and pe_1set_x01 = assign (Some((Some true )::set)) x01 in
(*			and pe_Nset_x01 = assign (Some( None       ::set)) x01 in *)
(*			let pe_set_x0_pe_set_x1 = pe_set_x0 *! pe_set_x1 in *)
			assert(pe_set_x0 = pe_0set_x01);
			assert(pe_set_x1 = pe_1set_x01);
(*			assert(pe_Nset_x01 = pe_set_x0_pe_set_x1); *)
			()
		) (gen_assign n)
	)
	| Utils.MNode (e, (xy, ix', iy')) ->
	(
		()
	);
	()
;;

Iter.iter (fun (ex, ey) ->
	let ix = if List.exists (function S -> true | _ -> false) ex.sub
		then Utils.Node 1
		else Utils.Leaf ()
	and iy, iy' = if List.exists (function S -> true | _ -> false) ey.sub
		then Utils.Node 1 , Utils.Node 2
		else Utils.Leaf (), Utils.Leaf ()
	in
	let x = (ex, ix)
	and y = (ey, iy)
	and y' = (ey, iy') in
	compatible_test_3_2 x y;
	compatible_test_3_2 x y')
	((gen_block_checked n) $* (gen_block_checked n));
