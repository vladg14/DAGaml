(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open Utils
open IterExtra
open Cpx2BTypes
open Cpx2BTypesTest
open Cpx2BUtils
open Cpx2BO3
open Cpx2BGops



let _ =
	let test_reversible     = true
	and test_compatible_3_2 = true
	and test_compatible_4_2 = true
	and test_and_eval       = true
	and test_xor_eval       = true
	and test_and_peval     = true
	and test_xor_peval     = true in


	let n = int_of_string(Sys.argv.(1)) in
	
	print_string "#block[";
	print_int n;
	print_string "] = ";
	print_int (Iter.length (gen_block_checked n));
	print_string ";";
	print_newline();

	print_string "TEST 3.1 : solve_cons"; print_newline();

	let reversible =
		TestBUtils.cons_is_reversible solve_cons node_pull arity_edge
	in

	let meta_test test = Iter.iter (fun ((block0, block1) : block * block) ->
		let node x = Node (None, x) in
		assert(block0.arity = block1.arity);
		if (check_block block0 false)
		then
		(
			if (check_block block1 false)
			then
			(
				test ((), (block0, node 0), (block1, node 0));
				test ((), (block0, node 0), (block1, node 1))
			);
			if (check_block block1 true)
			then
			(
				test ((), (block0, node 0), (block1, Leaf ()))
			)
		);
		if (check_block block0 true)
		then
		(
			if (check_block block1 false)
			then (test ((), (block0, Leaf()), (block1, node 1)));
			if (check_block block1 true)
			then (test ((), (block0, Leaf()), (block1, Leaf ())))
		)
	) (gen_block_checked n $* gen_block_checked n |> (Iter.progress ">>>" 10000 0))
	in

	if test_reversible then (meta_test reversible);


	print_string "TEST 3.2 : solve_cons using in_block"; print_newline();

	let compatible_test_3_2 (((), (ex, ix), (ey, iy)) as node) =
		let exy, merge = solve_cons node in
		match merge with
		| MEdge next ->
		(
			Iter.iter (fun vec ->
				assert(in_block exy (false::vec) = in_block ex vec);
				assert(in_block exy (true ::vec) = in_block ey vec);
				()
			) (gen_vec n)
		)
		| MNode ((), (ex', ix'), (ey', iy')) ->
		(
			let ok = function
				| None -> fun _ -> true
				| Some x -> function None -> false | Some y -> x = y
			in
			Iter.iter (fun vec ->
				assert(ok (in_block exy (false::vec)) (in_block ex vec));
				assert(ok (in_block exy (true ::vec)) (in_block ey vec));
				()
			) (gen_vec n)
		);
		()
	in

	if test_compatible_3_2 then (meta_test compatible_test_3_2);

	print_string "TEST 4.2 : check peval using solve_cons"; print_newline();

	let compatible_test_4_2 (((), ((e0, i0) as x0), ((e1, i1) as x1)) as node) =
		let blockC, merge = solve_cons node in
		match merge with
		| MEdge next ->
		(
			let x01 = (blockC, next) in
			Iter.iter (fun peval ->
				let pe_set_x0 = peval_pedge peval (Utils.pedge_of_edge x0)
				and pe_set_x1 = peval_pedge peval (Utils.pedge_of_edge x1)
				and pe_0set_x01 = peval_pedge ((Some false)::peval) (Utils.pedge_of_edge x01)
				and pe_1set_x01 = peval_pedge ((Some true )::peval) (Utils.pedge_of_edge x01) in
				assert(pe_set_x0 = pe_0set_x01);
				assert(pe_set_x1 = pe_1set_x01);
				()
			) (GUtils.gen_peval n)
		)
		| MNode ((), x0', x1') ->
		(
			()
		);
		()
	in
	
	if test_compatible_4_2 then (meta_test compatible_test_4_2);

	let peval_of_eval eval = List.map(fun x -> Some x)eval in

	let eval_pedge eval pedge =
		edge_is_const (peval_pedge (peval_of_eval eval) pedge)
	in


	let eval_merge3 solve eval merge3 =
		let edge, merge = peval_merge3 solve (peval_of_eval eval) merge3 in
		match merge with
			| M3Edge next -> edge_is_const (edge, next)
			| _ -> None
	in

	let conserve_eval solver binop (((), pedge0, pedge1) : _ pnode) =
		(* print_string "@@"; print_newline(); *)
		let (=>) x y = match x, y with
			| None  , _      -> true
			| Some _, None   -> false
			| Some x, Some y -> x = y
		in
		assert(arity_edge pedge0 = arity_edge pedge1);
		let merge3 = solver ((), pedge0, pedge1) in
		assert(check_merge3 merge3);
		Iter.iter (fun peval ->
			let e0 = eval_pedge         peval pedge0
			and e1 = eval_pedge         peval pedge1
			and m3 = eval_merge3 solver peval merge3 in
			if(not(binop (e0, e1) => m3))
			then
			(
				Cpx2BDump.edge pedge0 |> print_string;
				print_string " <op> ";
				Cpx2BDump.edge pedge1 |> print_string;
				print_string " = ";
				Cpx2BDump.emerge3 merge3 |> print_string;
				print_newline();
				assert(false)
			)) (Iter.gen_bool $^ (arity_edge pedge0))
	in

	print_string "TEST 5.1 : check AND using eval"; print_newline();
	let opand = function
		| Some true , Some true  -> Some true
		| Some false, _
		| _         , Some false -> Some false
		| _                      -> None
	in
	let opxor = function
		| Some x, Some y -> Some(x<>y)
		| _              -> None
	in
	let pedge0 = ( {neg = false; arity = 3; block = SPX(true, {hasS = false; hasP = true; maxX = Some 0}, [P; X(true, 0); X(true, 0)])}, Leaf() )
	and pedge1 = ( {neg = false; arity = 3; block = SPX(true, {hasS = false; hasP = true; maxX = Some 0}, [X(true, 0); P; X(true, 0)])}, Leaf() ) in
	conserve_eval solve_and opand ((), pedge0, pedge1);

	if test_and_eval then (meta_test (conserve_eval solve_and opand));

	print_string "TEST 5.2 : check XOR using eval"; print_newline();
	let pedge0 = ( {neg = true; arity = 3; block = SPX(true, {hasS = false; hasP = true; maxX = Some 0}, [P; X(true, 0); X(true, 0)])}, Leaf() )
	and pedge1 = ( {neg = true; arity = 3; block = SPX(true, {hasS = false; hasP = true; maxX = Some 0}, [X(true, 0); P; X(true, 0)])}, Leaf() ) in
	conserve_eval solve_xor opxor ((), pedge0, pedge1);
	let pedge0 = ( {neg = true; arity = 3; block = SPX(true, {hasS = true; hasP = false; maxX = Some 1}, [X(true, 1); S; X(true, 0)])}, Node (None, 0) )
	and pedge1 = ( {neg = true; arity = 3; block = SPX(true, {hasS = false; hasP = false; maxX = Some 1}, [X(false, 1); X(true, 1); X(true, 0)])}, Node (None, 0) ) in
	conserve_eval solve_xor opxor ((), pedge0, pedge1);
	if test_xor_eval then (meta_test (conserve_eval solve_xor opxor));
	
	let conserve_peval (solver : 'i pnode -> 'i plink emerge3) (((), pedge0, pedge1) as pnode) =
		(* print_string "@@"; print_newline(); *)
		assert(arity_edge pedge0 = arity_edge pedge1);
		let merge3 : _ plink emerge3 = solver ((), pedge0, pedge1) in
		assert(check_merge3 merge3);
		Iter.iter (fun peval ->
			let e0  = peval_pedge  peval pedge0
			and e1  = peval_pedge  peval pedge1
			and m3  = peval_merge3 solver peval merge3 in
			let m3' = solver ((), e0, e1) in
			if(not(m3 = m3'))
			then
			(
				print_string "(* ";
				print_node "$A-" pnode;
				print_string " *)"; print_newline();
				print_string "\nlet pedge0 = ";
				Cpx2BDump.edge pedge0 |> print_string;
				print_string " (* ";
				BinDump.(closure bool_option_list peval |> StrDump.bitv_hexa) |> print_string;
				print_string " *) ";
				print_string "\nand pedge1 = ";
				Cpx2BDump.edge pedge1 |> print_string;
				print_string "\nand merge3 = ";
				Cpx2BDump.emerge3 merge3 |> print_string;
				print_string "\nand m3  = ";
				Cpx2BDump.emerge3 m3 |> print_string;
				print_string "\nand e0  = ";
				Cpx2BDump.edge e0 |> print_string;
				print_string "\nand e1  = ";
				Cpx2BDump.edge e1 |> print_string;
				print_string "\nand m3' = ";
				Cpx2BDump.emerge3 m3' |> print_string;
				print_string " in";
				print_newline();
				assert(false)
			)) (GUtils.gen_peval (arity_edge pedge0))
	in
	print_string "TEST 5.2 : check AND using peval"; print_newline();
	let pedge0 = ( {neg = true; arity = 2; block = SPX(false, {hasS = true; hasP = false; maxX = Some (0)}, [X(true, 0); S])}, Node (None, 0))
	and pedge1 = ( {neg = true; arity = 2; block = SPX(false, {hasS = false; hasP = false; maxX = Some (0)}, [X(false, 0); X(true, 0)])}, Node (None, 0)) in
	conserve_peval solve_and ((), pedge0, pedge1);
	let pedge0 = ( {neg = true; arity = 2; block = SPX(true, {hasS = false; hasP = false; maxX = Some (0)}, [X(true, 0); X(true, 0)])}, Node (None, 0))
	and pedge1 = ( {neg = true; arity = 2; block = SPX(true, {hasS = false; hasP = false; maxX = Some (1)}, [X(true, 0); X(true, 1)])}, Node (None, 0)) in
(*
	and merge3 = M3Node(
		{neg = false; arity = 2; block = SPX(false, {hasS = true; hasP = false; maxX = Some (0)}, [X(true, 0); S])},
		( {neg = false; arity = 1; block = SPX(false, {hasS = false; hasP = false; maxX = Some (0)}, [X(true, 0)])}, Node ),
		( {neg = false; arity = 1; block = SPX(true , {hasS = false; hasP = false; maxX = Some (0)}, [X(true, 0)])}, Node ) )
	and m3  = M3Edge( ( {neg = true; arity = 2; block = SPX(true, {hasS = false; hasP = false; maxX = Some (0)}, [X(true, 0); X(false, 0)])}, Leaf ) )
	and e0  = ( {neg = true; arity = 2; block = SPX(true, {hasS = false; hasP = false; maxX = Some (0)}, [X(true, 0); X(true, 0)])}, Node )
	and e1  = ( {neg = true; arity = 2; block = SPX(true, {hasS = false; hasP = false; maxX = Some (1)}, [X(true, 0); X(true, 1)])}, Node )
	and m3' = M3Node(
		{neg = false; arity = 2; block = SPX(false, {hasS = true; hasP = false; maxX = Some (0)}, [X(true, 0); S])},
		( {neg = false; arity = 1; block = SPX(false, {hasS = false; hasP = false; maxX = Some (0)}, [X(true, 0)])}, Node ),
		( {neg = false; arity = 1; block = SPX(true , {hasS = false; hasP = false; maxX = Some (0)}, [X(true, 0)])}, Node ) ) in
*)
	conserve_peval solve_and ((), pedge0, pedge1);



	if test_and_peval then (meta_test (conserve_peval solve_and));
	print_string "TEST 5.2 : check XOR using peval"; print_newline();
	let pedge0 = ( {neg = true; arity = 1; block = SPX(true, {hasS = false; hasP = false; maxX = Some 0}, [X(true, 0)])}, Node (None, 0) )
	and pedge1 = ( {neg = true; arity = 1; block = SPX(true, {hasS = false; hasP = false; maxX = Some 0}, [X(true, 0)])}, Node (None, 1) ) in
	conserve_peval solve_xor ((), pedge0, pedge1);

	if test_xor_peval then (meta_test (conserve_peval solve_xor));
	
	()
	

	
;;
