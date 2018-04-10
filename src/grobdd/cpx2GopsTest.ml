(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open IterExtra
open Cpx2Types
open Cpx2TypesTest
open Cpx2Utils
open Cpx2DumpLoad
open Cpx2Gops



let _ =
	let test_reversible     = true
	and test_compatible_3_2 = true
	and test_compatible_4_2 = true
	and test_and_eval       = true
	and test_xor_eval       = true
	and test_and_assign     = true
	and test_xor_assign     = true in


	let n = int_of_string(Sys.argv.(1)) in
	
	print_string "#block[";
	print_int n;
	print_string "] = ";
	print_int (Iter.length (gen_block_checked n));
	print_string ";";
	print_newline();

	print_string "TEST 3.1 : solve_cons"; print_newline();

	let reversible ((block0, node0) as edge0) ((block1, node1) as edge1) =
		assert(block0.arity = block1.arity);
		match (solve_cons edge0 edge1 : (_, _) Utils.merge) with
		| Utils.MEdge edge01 ->
		( match node_pull edge01 with
			| Utils.MEdge (edge0', edge1') -> assert((edge0 = edge0')&&(edge1 = edge1'))
			| Utils.MNode _ -> assert false
		)
		| Utils.MNode (edge01, (node, node0', node1')) ->
		(
			assert(node0' = node0);
			assert(node1' = node1);
			(
				let stream = bindump_node node in
				let node' = binload_node stream in
				assert(node' = node)
			);
			match node_pull (edge01, Utils.Node 3) with
				| Utils.MEdge _ -> assert false
				| Utils.MNode f ->
				(
					let edge0', edge1' = f (node, node0', node1') in
					assert(edge0 = edge0' && edge1 = edge1')
				)
		);
		()
	in

	let meta_test test = Iter.iter (fun (block0, block1) ->
		let node x = Utils.Node (None, x) in
		assert(block0.arity = block1.arity);
		if (check_block block0 false)
		then
		(
			if (check_block block1 false)
			then
			(
				test (block0, node 0) (block1, node 0);
				test (block0, node 0) (block1, node 1)
			);
			if (check_block block1 true)
			then (reversible (block0, node 0) (block1, Utils.Leaf ()))
		);
		if (check_block block0 true)
		then
		(
			if (check_block block1 false)
			then (test (block0, Utils.Leaf()) (block1, node 1));
			if (check_block block1 true)
			then (test (block0, Utils.Leaf()) (block1, Utils.Leaf ()))
		)
	) (Iter.progress ">>>" 1000 0 ((gen_block_checked n) $* (gen_block_checked n)))
	in

	if test_reversible then (meta_test reversible);


	print_string "TEST 3.2 : solve_cons using in_block"; print_newline();

	let compatible_test_3_2 ((ex, ix) as x) ((ey, iy) as y) =
		match solve_cons x y with
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
	in

	if test_compatible_3_2 then (meta_test compatible_test_3_2);

	print_string "TEST 4.2 : check assign using solve_cons"; print_newline();

	let compatible_test_4_2 ((e0, i0) as x0) ((e1, i1) as x1) =
		match solve_cons x0 x1 with
		| Utils.MEdge ((e01, i01) as x01) ->
		(
			Iter.iter (fun set ->
				let pe_set_x0 = assign_edge (Some set) x0
				and pe_set_x1 = assign_edge (Some set) x1
				and pe_0set_x01 = assign_edge (Some((Some false)::set)) x01
				and pe_1set_x01 = assign_edge (Some((Some true )::set)) x01 in
				assert(pe_set_x0 = pe_0set_x01);
				assert(pe_set_x1 = pe_1set_x01);
				()
			) (gen_assign n)
		)
		| Utils.MNode (e, (xy, ix', iy')) ->
		(
			()
		);
		()
	in
	
	if test_compatible_4_2 then (meta_test compatible_test_4_2);

	let assign_merge3 solve peval = function
	| Utils.M3Edge pedge -> Utils.M3Edge(assign_pedge peval pedge)
	| Utils.M3Cons (blockC, (pedge0, pedge1)) ->
	(
		assert(arity_edge pedge0 = arity_edge pedge1);
		let (blockC', pnode') = assign_pedge peval (blockC, Utils.Node (None, ())) in
		match pnode' with
		| Utils.Leaf () -> Utils.M3Edge(blockC', Utils.Leaf())
		| Utils.Node (peval, ()) -> match peval with
			| None -> Utils.M3Cons(blockC', (pedge0, pedge1))
			| Some [] -> assert(false)
			| Some (head::peval) -> match head with
				| None ->
				(
					let pedge0 = assign_pedge (Some peval) pedge0
					and pedge1 = assign_pedge (Some peval) pedge1 in
				 	match compose_utils_merge blockC' (solve_cons pedge0 pedge1) with
					| Utils.MEdge pedge -> Utils.M3Edge pedge
					| Utils.MNode (blockC, ((block0, block1), pnode0, pnode1)) ->
						Utils.M3Cons (blockC, ((block0, pnode0), (block1, pnode1)))
				)
				| Some choice -> Utils.M3Edge(compose_edge blockC' (assign_pedge (Some peval) (if choice then pedge1 else pedge0)))
	)
	| Utils.M3Node (blockC, ((block0, block1), pnode0, pnode1)) ->
	(
		assert(block0.arity = block1.arity);
		let (blockC', pnode') = assign_pedge peval (blockC, Utils.Node (None, ())) in
		match pnode' with
		| Utils.Leaf () -> Utils.M3Edge (blockC', Utils.Leaf())
		| Utils.Node (peval, ()) ->
			let pedge0 = assign_pedge peval (block0, pnode0)
			and pedge1 = assign_pedge peval (block1, pnode1) in
			compose_utils_merge3 blockC' (solve pedge0 pedge1)
	)
	in

	let eval_pedge eval pedge =
		let block, pnode = assign_pedge (Some(List.map(fun x -> Some x)eval)) pedge in
		if block.block = C0 then (assert(pnode = Utils.Leaf()); (Some block.neg)) else None
	in

	let eval_merge3 solve eval merge3 = match assign_merge3 solve (Some(List.map(fun x -> Some x)eval)) merge3 with
		| Utils.M3Edge (block, Utils.Leaf()) -> (if block.block = C0 then (Some block.neg) else None)
		| _ -> None
	in

	let conserve_eval solver binop pedge0 pedge1 =
		(* print_string "@@"; print_newline(); *)
		let (=>) x y = match x, y with
			| None  , _      -> true
			| Some _, None   -> false
			| Some x, Some y -> x = y
		in
		assert(arity_edge pedge0 = arity_edge pedge1);
		let merge3 : (_, _, _) Utils.merge3 = solver pedge0 pedge1 in
		assert(check_utils_merge3 merge3);
		Iter.iter (fun eval ->
			let e0 = eval_pedge  eval pedge0
			and e1 = eval_pedge  eval pedge1
			and m3 = eval_merge3 solver eval merge3 in
			if(not(binop (e0, e1) => m3))
			then
			(
				dummydump_edge pedge0 |> print_string;
				print_string " <op> ";
				dummydump_edge pedge1 |> print_string;
				print_string " = ";
				dummydump_merge3 merge3 |> print_string;
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
	let pedge0 = ( {neg = false; arity = 3; block = SPX(true, {hasS = false; hasP = true; maxX = Some 0}, [P; X(true, 0); X(true, 0)])}, Utils.Leaf() )
	and pedge1 = ( {neg = false; arity = 3; block = SPX(true, {hasS = false; hasP = true; maxX = Some 0}, [X(true, 0); P; X(true, 0)])}, Utils.Leaf() ) in
	conserve_eval solve_and opand pedge0 pedge1;

	if test_and_eval then (meta_test (conserve_eval solve_and opand));
	print_string "TEST 5.2 : check XOR using eval"; print_newline();
	let pedge0 = ( {neg = true; arity = 3; block = SPX(true, {hasS = false; hasP = true; maxX = Some 0}, [P; X(true, 0); X(true, 0)])}, Utils.Leaf() )
	and pedge1 = ( {neg = true; arity = 3; block = SPX(true, {hasS = false; hasP = true; maxX = Some 0}, [X(true, 0); P; X(true, 0)])}, Utils.Leaf() ) in
	conserve_eval solve_xor opxor pedge0 pedge1;
	let pedge0 = ( {neg = true; arity = 3; block = SPX(true, {hasS = true; hasP = false; maxX = Some 1}, [X(true, 1); S; X(true, 0)])}, Utils.Node (None, 0) )
	and pedge1 = ( {neg = true; arity = 3; block = SPX(true, {hasS = false; hasP = false; maxX = Some 1}, [X(false, 1); X(true, 1); X(true, 0)])}, Utils.Node (None, 0) ) in
	conserve_eval solve_xor opxor pedge0 pedge1;
	if test_xor_eval then (meta_test (conserve_eval solve_xor opxor));

	let conserve_assign solver pedge0 pedge1 =
		(* print_string "@@"; print_newline(); *)
		assert(arity_edge pedge0 = arity_edge pedge1);
		let merge3 : (_, _, _) Utils.merge3 = solver pedge0 pedge1 in
		assert(check_utils_merge3 merge3);
		Iter.iter (fun peval ->
			let e0  = assign_pedge  (Some peval) pedge0
			and e1  = assign_pedge  (Some peval) pedge1
			and m3  = assign_merge3 solver (Some peval) merge3 in
			let m3' = solver e0 e1 in
			if(not(m3 = m3'))
			then
			(
				print_string "\nlet pedge0 = ";
				dummydump_edge pedge0 |> print_string;
				print_string "\nand pedge1 = ";
				dummydump_edge pedge1 |> print_string;
				print_string "\nand merge3 = ";
				dummydump_merge3 merge3 |> print_string;
				print_string "\nand m3  = ";
				dummydump_merge3 m3 |> print_string;
				print_string "\nand e0  = ";
				dummydump_edge e0 |> print_string;
				print_string "\nand e1  = ";
				dummydump_edge e1 |> print_string;
				print_string "\nand m3' = ";
				dummydump_merge3 m3' |> print_string;
				print_string " in";
				print_newline();
				assert(false)
			)) (gen_assign (arity_edge pedge0))
	in
	print_string "TEST 5.2 : check AND using assign"; print_newline();
	let pedge0 = ( {neg = true; arity = 2; block = SPX(false, {hasS = true; hasP = false; maxX = Some (0)}, [X(true, 0); S])}, Utils.Node (None, 0))
	and pedge1 = ( {neg = true; arity = 2; block = SPX(false, {hasS = false; hasP = false; maxX = Some (0)}, [X(false, 0); X(true, 0)])}, Utils.Node (None, 0)) in
	conserve_assign solve_and pedge0 pedge1;
	let pedge0 = ( {neg = true; arity = 2; block = SPX(true, {hasS = false; hasP = false; maxX = Some (0)}, [X(true, 0); X(true, 0)])}, Utils.Node (None, 0))
	and pedge1 = ( {neg = true; arity = 2; block = SPX(true, {hasS = false; hasP = false; maxX = Some (1)}, [X(true, 0); X(true, 1)])}, Utils.Node (None, 0)) in
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
	conserve_assign solve_and pedge0 pedge1;



	if test_and_assign then (meta_test (conserve_assign solve_and));
	print_string "TEST 5.2 : check XOR using assign"; print_newline();
	let pedge0 = ( {neg = true; arity = 1; block = SPX(true, {hasS = false; hasP = false; maxX = Some 0}, [X(true, 0)])}, Utils.Node (None, 0) )
	and pedge1 = ( {neg = true; arity = 1; block = SPX(true, {hasS = false; hasP = false; maxX = Some 0}, [X(true, 0)])}, Utils.Node (None, 1) ) in
	conserve_assign solve_xor pedge0 pedge1;

	if test_xor_assign then (meta_test (conserve_assign solve_xor));
	
	()
	

	
;;
