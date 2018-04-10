(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open IterExtra
open Cpx2BTypes
open Cpx2BTypesTest
open Cpx2BUtils

let n = int_of_string(Sys.argv.(1));;

print_string "TEST 1.0 : contig"; print_newline();
(*Iter.iter (fun x -> print_string (dummydump x); print_newline()) (gen1 n);*)
print_string "LENGTH = "; print_int (Iter.length (gen1 n)); print_newline();
print_string "TEST 2.0 : block"; print_newline();
(*Iter.iter (fun x -> print_string (block_dummydump x); print_newline()) (gen_block n);*)
print_string "LENGTH = "; print_int (Iter.length (gen_block n)); print_newline();
print_string "TEST 2.1 : reduced block are correct"; print_newline();
Iter.iter (fun block0 ->
	let block2 = reduce_block block0 false in
	(assert_block block2 false);
	if not (hasS block0)
	then
	(
		let block1 = reduce_block block0 true in
		assert(check_block block1 true);
		assert(not(hasS block2));
		let block3 = reduce_block block2 true in
		assert(block1 = block3)
	);
) (gen_block n);

print_string "TEST 2.2 : reduce^2 = reduce1"; print_newline();
Iter.iter (fun x ->
	let y0 = reduce_block x  false in
	let z0 = reduce_block y0 false in
	assert(y0 = z0);
	if not (hasS x)
	then
	(
		let y1 = reduce_block x  true in
		let z1 = reduce_block y1 true in
		assert(y1 = z1)
	)
	) (gen_block n);


print_string "TEST 2.3 : reduce using in_block"; print_newline();
Iter.iter (fun block0 ->
	let in0 = in_block block0 in
	if not(hasS block0)
	then
	(
		let block1 = reduce_block block0 true in
		let in1 = in_block block1 in
		if block0 <> block1 then
		(
			Iter.iter (fun vec -> assert(in0 vec = in1 vec)) (gen_vec n)
		)
	);
	let block2 = reduce_block block0 false in
	let in2 = in_block block2 in
	if block0 <> block2 then
	(
		Iter.iter (fun vec -> assert(in0 vec = in2 vec)) (gen_vec n)
	)
) (gen_block n);

print_string "TEST 3.0 : checked block"; print_newline();
(*Iter.iter (fun x -> print_string (block_to_pretty x); print_newline(); print_newline()) (gen_block_checked n);*)
print_string "LENGTH = "; print_int (Iter.length (gen_block_checked n)); print_newline();
print_string "TEST 3.0.1 : reduce on correct block is identity"; print_newline();
Iter.iter (fun block0 ->
	if (check_block block0 true)
	then (
		let block1 = reduce_block block0 true  in
		assert(block0 = block1)
	);
	if (check_block block0 false)
	then (
		let block2 = reduce_block block0 false in
		assert(block0 = block2)
	)
) (gen_block n);;

print_string "TEST 5.0 : check compose : assert(check A o B)"; print_newline();
Iter.iter (fun blockA ->
	let nSA = count_nS_block blockA in
	assert(check_block blockA false);
	Iter.iter (fun blockB ->
		assert(check_block blockB false);
		if (check_block blockB false)
		then
		(
			let blockAB = compose_block blockA blockB false in
			assert(check_block blockAB false)
		);
		if (check_block blockB true)
		then
		(
			let blockAB = compose_block blockA blockB true in
			assert(check_block blockAB true)
		)

	) (gen_block_checked nSA)
) (gen_block_checked n);;

print_string "TEST 5.1 : check compose associativity : A o (B o C) = (A o B) o C"; print_newline();
Iter.iter (fun blockA ->
	let nSA = count_nS_block blockA in
	assert(check_block blockA false);
	Iter.iter (fun blockB ->
		assert(check_block blockB false);
		let nSB = count_nS_block blockB in
		let blockAB = compose_block blockA blockB false in
		Iter.iter (fun blockC ->
			assert(check_block blockC false);
			if (check_block blockC false)
			then
			(
				let blockBC = compose_block blockB blockC false in
				let blockAB_C = compose_block blockAB blockC false in
				let blockA_BC = compose_block blockA blockBC false in
				assert(blockAB_C = blockA_BC)
			);
			if (check_block blockC true)
			then
			(
				let blockBC = compose_block blockB blockC true in
				let blockAB_C = compose_block blockAB blockC true in
				let blockA_BC = compose_block blockA blockBC true in
				assert(blockAB_C = blockA_BC)
			)
		) (gen_block_checked nSB)
	) (gen_block_checked nSA)
) (gen_block_checked n);;

print_string "TEST 4.0 : check assign returns correct block"; print_newline();;


Iter.iter (fun (block : block) ->
	if (check_block block false)
	then
	(Iter.iter (fun peval ->
		let edge, _ = peval_edge peval (block, Utils.Node 0) in
		assert(check_edge edge)
	) (gen_assign n));
	if (check_block block true )
	then
	(Iter.iter (fun peval ->
		let edge, _ = peval_edge peval (block, Utils.Leaf ()) in
		assert(check_edge edge)
	) (gen_assign n));
	()
) (gen_block_checked n);;

print_string "TEST 4.1 : check assign & compose compatibility"; print_newline();;


Iter.iter (fun blockC ->
	let nS = count_nS_block blockC in
	if check_block blockC false then(
	Iter.iter (fun peval ->
		let (blockC', identC') as edgeC', peval' = peval_edge peval (blockC, Utils.Node 1) in
		assert(check_edge edgeC');
		match identC' with
		| Utils.Leaf () -> ()
		| Utils.Node x -> assert(x=1);
		(
			Iter.iter (fun blockc ->
				if check_block blockc false
				then
				(
					let nodec = Utils.Node 0 in
					let edgec = (blockc, nodec) in
					assert(check_edge edgec);
					let edgec', (peval'' : bool option list option) = opeval_edge peval' edgec in
					assert(check_edge edgec');
					let edgeC'c' = compose_edge blockC' edgec' in
					assert(check_edge edgeC'c');
					let edgeCc = compose_edge blockC edgec in
					assert(check_edge edgeCc);
					let edgeCc', (peval'3 : bool option list option) = peval_edge peval edgeCc in
					assert(check_edge edgeCc');
					assert(edgeC'c' = edgeCc' && peval'' = peval'3);
					
				);
				if check_block blockc true
				then
				(
					let nodec = Utils.Leaf () in
					let edgec = (blockc, nodec) in
					assert(check_edge edgec);
					let edgec', (peval'' : bool option list option) = opeval_edge peval' edgec in
					assert(check_edge edgec');
					let edgeC'c' = compose_edge blockC' edgec' in
					assert(check_edge edgeC'c');
					let edgeCc = compose_edge blockC edgec in
					assert(check_edge edgeCc);
					let edgeCc', (peval'3 : bool option list option) = peval_edge peval edgeCc in
					assert(check_edge edgeCc');
					assert(edgeC'c' = edgeCc' && peval'' = peval'3);
				)
			) (gen_block_checked nS)
		)
	) (gen_assign n))
) (gen_block_checked n);;
