(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open Cpx2Types
open Cpx2Utils

module CpxDL = Cpx2DumpLoad


let ddl_cons = false
let ddl_and  = false
let ddl_xor  = false


let block_is_singleton block = match block.block with Id _ -> true | _ -> false

let solve_cons_id_spx_no_merge x shift liste =
	let rec aux carryC carryc i = function
		| [] -> (List.rev carryC, List.rev carryc)
		| head::tail -> match head with
			| P when i<>x  -> (aux (P::carryC)        carryc  (i+1) tail)
			| _            -> (aux (S::carryC) (head::carryc) (i+1) tail)
	in
	let listeC, listec = aux [] [] 0 liste in
	let listeC = S::listeC in
	let tagC = get_spx_tag_from_block_spx listeC
	and tagc = get_spx_tag_from_block_spx listec in
	let rec aux n i = function
		| [] -> assert false
		| head::tail -> if i = x then n else match head with
			| P -> aux  n    (i+1) tail
			| _ -> aux (n+1) (i+1) tail
	in
	(false, tagC, listeC), (List.length listec), (aux 0 0 liste), (shift, tagc, listec)
	

let meta_solve_cons' ((block0, node0) as edge0) ((block1, node1) as edge1) ifspx2 =
	if edge0 = edge1
	then (Utils.MEdge (push_P_edge edge0))
	else match block0.block, block1.block with
	| C0      , _        -> Utils.MEdge(push_X0_edge false block0.neg edge1)
	| _       , C0       -> Utils.MEdge(push_X0_edge true  block1.neg edge0)
	| Id x0   , Id x1    ->
	(
		if x0 = x1
		then
		(
			assert(block0.neg <> block1.neg);
			let liste = S::(MyList.init block0.arity (fun i -> if i = x0 then S else P)) in
			let tag = {hasS = true; hasP = block0.arity > 1; maxX = None} in
			let block_spx = (false, tag, liste) in
			Utils.MNode (
				{neg = block0.neg; arity = block0.arity+1; block = SPX block_spx},
				((
					{neg = false; arity = 1; block = Id 0},
					{neg = true ; arity = 1; block = Id 0}
				), node0, node1)
			)
		)
		else
		(
			let liste = S::(MyList.init block0.arity (fun i -> if i = x0 || i = x1 then S else P)) in
			let id0, id1 = if x0 < x1 then (0, 1) else (1, 0) in
			let tag = {hasS = true; hasP = block0.arity > 2; maxX = None} in
			let block_spx = (false, tag, liste) in
			Utils.MNode (
				{neg = block0.neg; arity = block0.arity+1; block = SPX block_spx},
				((
					{neg = false; arity = 2; block = Id id0},
					{neg = block0.neg <> block1.neg; arity = 2; block = Id id1}
				), node0, node1)
			)
		)
	) 
	| Id x   , SPX (shift, _, liste) ->
	(
		match List.nth liste x with
		| X(b, 0) when (b <> block0.neg) = (shift <> block1.neg) ->
		(
			let liste' = (X(false, 1))::(List.mapi (fun y -> function X(b, i) when x<>y -> X(b, i+2) | e -> e) liste) in
			let block = reduce_block_spx block1.neg (block1.arity+1) (Utils.gnode_is_leaf node1) shift liste' in
			Utils.MEdge (block, node1)
		)
		| _ ->
		(
			let spxC, (arityc:int), (xc:int), spxc = solve_cons_id_spx_no_merge x shift liste in
			Utils.MNode (
				{neg = block0.neg; arity = block0.arity+1; block = SPX spxC},
				((
					{neg = false; arity = arityc; block = Id xc},
					{neg = block0.neg <> block1.neg; arity = arityc; block = SPX spxc}
				), node0, node1)
			)
		)
	)
	| SPX (shift, _, liste), Id x    ->
	(
		match List.nth liste x with
		| X(b, 0) when (shift <> block0.neg) = (b <> block1.neg) ->
		(
			let liste' = (X(true, 1))::(List.mapi (fun y -> function X(b, i) when x<>y -> X(b, i+2) | e -> e) liste) in
			let block = reduce_block_spx block0.neg (block0.arity+1) (Utils.gnode_is_leaf node0) shift liste' in
			Utils.MEdge (block, node0)
		)
		| _ ->
		(
			let spxC, arityc, xc, spxc = solve_cons_id_spx_no_merge x shift liste in
			Utils.MNode (
				{neg = block0.neg; arity = block0.arity+1; block = SPX spxC},
				((
					{neg = false; arity = arityc; block = SPX spxc},
					{neg = block0.neg <> block1.neg; arity = arityc; block = Id xc}
				), node0, node1)
			)
		)
	)
	| SPX spx0, SPX spx1 -> ifspx2 block0.arity block0.neg block1.neg (spx0, node0) (spx1, node1)


let meta_solve_cons ((block0, node0) as edge0) ((block1, node1) as edge1) ifspx2 =
	assert(check_edge edge0);
	assert(check_edge edge1);
	assert(block0.arity = block1.arity);
	let result = meta_solve_cons' edge0 edge1 ifspx2 in
	(match result with
	| Utils.MEdge ((block, node) as edge) ->
	(
		assert(node0 = Utils.Leaf () || node1 = Utils.Leaf () || node0 = node1);
		assert(node = node0 || node = node1);
		assert(block.arity = block0.arity+1);
		assert(check_edge edge);
		()
	)
	| Utils.MNode (blockC, ((block0', block1'), node0', node1')) ->
	(
		assert(node0 = node0');
		assert(node1 = node1');
		assert(block0'.arity = block1'.arity);
		assert(block0'.block <> C0);
		assert(block1'.block <> C0);
		assert(blockC.arity  = block0.arity+1);
		assert(check_block blockC false);
		assert(check_edge (block0', node0'));
		assert(check_edge (block1', node1'));
		()
	));
	result


let final_cons_ifspx2 arity neg0 neg1 (spx0, i0) (spx1, i1) =
	Utils.MNode (
		make_block_S neg0 (arity+1),
		((
			{neg = false     ; arity; block = SPX spx0},
			{neg = neg0<>neg1; arity; block = SPX spx1}
		), i0, i1)
	)

let facto_cons_ifspx2 arity neg0 neg1 ((s0, t0, l0), i0) ((s1, t1, l1), i1) =
	match t0.maxX, t1.maxX with
	| Some max0, Some max1 when (neg0 <> s0) = (neg1 <> s1) ->
	(
		let opmin = List.fold_left (fun opmin -> function
			| X(b, i), X(b', i') -> if (b=b')&&(i=i')
				then opmin
				else (Tools.opmin (min i i') opmin)
			| X(_, i), _
			| _      , X(_, i) -> Tools.opmin i opmin
			| _                -> opmin) None (List.combine l0 l1) in
		let shift, cfun = match opmin with
			| None -> false, (function (P, P) -> P, None | (X bi, X bi') -> assert(bi = bi'); X bi, None | (x, y) -> (S, Some(x, y)))
			| Some min ->
			(
				let remapX = function X(b, i) -> X(b, i-min) | e -> e in
				(mod2 min), (function
					| P, P -> P, None
					| X(b, i), X(b', i') when (b=b')&&(i=i')&&(i<=min) -> X(b, i), None
					| (x, y) -> S, Some (remapX x, remapX y)
				)
			)
		in
		let listeC, (listec0, listec1) = GUtils.consensus cfun l0 l1 in
		let arity' = List.length listec0 in
		assert(arity' = List.length listec1);
		let blockC = reduce_block_spx neg0         (arity+1)   false                    s0         (S::listeC)
		and block0 = reduce_block_spx false         arity'    (Utils.gnode_is_leaf i0) (s0<>shift)     listec0
		and block1 = reduce_block_spx (neg1<>neg0)  arity'    (Utils.gnode_is_leaf i1) (s1<>shift)     listec1 in
		compose_utils_merge blockC (meta_solve_cons (block0, i0) (block1, i1) final_cons_ifspx2)
	)
	| _ ->
	(
		let cfun = function (P, P) -> P, None | (x, y) -> (S, Some(x, y)) in
		let listeC, (listec0, listec1) = GUtils.consensus cfun l0 l1 in
		let arity' = List.length listec0 in
		assert(arity' = List.length listec1);
		let blockC = reduce_block_spx neg0         (arity+1) false                    false (S::listeC)
		and block0 = reduce_block_spx false         arity'  (Utils.gnode_is_leaf i0) s0         listec0
		and block1 = reduce_block_spx (neg1<>neg0)  arity'  (Utils.gnode_is_leaf i1) s1         listec1 in
		assert(block0.neg = false);
		Utils.MNode(blockC, ((block0, block1), i0, i1))
	)

let solve_cons edge0 edge1 : (_, _) Utils.merge =
	assert(check_edge edge0);
	assert(check_edge edge1);
	assert(arity_edge edge0 = arity_edge edge1);
	let merge = meta_solve_cons edge0 edge1 facto_cons_ifspx2 in
	merge


let pull_X i neg arity shift liste next_is_leaf =
	let mod2i = mod2 i in
	let liste_then = List.map (function (X(_, j)) as e when j < i -> e | _ -> P) liste in
	let block_then = reduce_block_spx (neg <> shift <> mod2i) (arity-1) true mod2i liste_then in
	let block_else = if List.for_all (function X(_, j) -> i<>j | _ -> true) liste
	then if i = 0
		then
		(
			let liste = List.map (function X(b, j) -> X(b, j-1) | e -> e) liste in
			reduce_block_spx neg (arity-1) next_is_leaf (not shift) liste
		)
		else
		(
			let liste = List.map (function X(b, j) when j > i -> X(b, j-2) | e -> e) liste in
			reduce_block_spx neg (arity-1) next_is_leaf shift liste
		)
	else (reduce_block_spx neg (arity-1) next_is_leaf shift liste) in
	(block_then, block_else)

let node_pull ((block, node) as edge) =
	assert(block.arity > 0);
	assert(check_edge edge);
	match block.block with
	| C0 ->
	(
		assert(node = Utils.Leaf());
		let block' = {neg = block.neg; arity = block.arity-1; block = C0} in
		Utils.MEdge ((block', Utils.Leaf()), (block', Utils.Leaf()))
	)
	| Id 0 ->
	(
		assert(node = Utils.Leaf());
		let block0 = {neg = block.neg; arity = block.arity-1; block = C0}
		and block1 = {neg = not block.neg; arity = block.arity-1; block = C0} in
		Utils.MEdge ((block0, Utils.Leaf()), (block1, Utils.Leaf()))
	)
	| Id x ->
	(
		assert(node = Utils.Leaf());
		assert(x > 0);
		let block' = {neg = block.neg; arity = block.arity-1; block = Id(x-1)} in
		Utils.MEdge ((block', Utils.Leaf()), (block', Utils.Leaf()))
	)
	| SPX(shift, tag, liste) -> match liste with
		| [] -> assert false
		| elem::liste' ->
		(
			let block' = reduce_block_spx block.neg (block.arity-1) (Utils.gnode_is_leaf node) shift liste' in
			match elem with
			| P -> Utils.MEdge ((block', node), (block', node))
			| S -> Utils.MNode (fun node ->
				let edge0, edge1 = node_split node in
				(compose_edge block' edge0, compose_edge block' edge1)
			)
			| X(b, i) ->
				let block_then, block_else = pull_X i block.neg block.arity shift liste' (Utils.gnode_is_leaf node) in
				let edge_then = (block_then, Utils.Leaf())
				and edge_else = (block_else, node) in
				Utils.MEdge (Tools.cswap b (edge_then, edge_else))
		)

let solve_and_id_pedge arity neg0 x0 pedge1 =
	let peval = Some(MyList.init arity (fun i -> if i = x0 then (Some(not neg0)) else None)) in
	let liste = MyList.init arity (fun i -> if i = x0 then (X(neg0, 0)) else S) in
	let tag = {hasP = false; hasS = arity>1; maxX = Some 0} in
	let blockC = {neg = false; arity = arity; block = SPX (false, tag, liste)} in
	assert(check_block blockC false);
	Utils.M3Edge(compose_edge blockC (assign_pedge peval pedge1))


let meta_solve_and ((block0, pnode0) as edge0) ((block1, pnode1) as edge1) ifspx2 =
	if ddl_and then (
		print_string "meta: ";
		CpxDL.dummydump_edge edge0 |> print_string;
		print_string " && ";
		CpxDL.dummydump_edge edge1 |> print_string;
		print_newline()
	);
	assert(check_edge edge0);
	assert(check_edge edge1);
	assert(block0.arity = block1.arity);
	match block0.block, block1.block with
	| C0      , _        ->
	(Utils.M3Edge(if block0.neg
		then edge1
		else (make_edge_C0 false block1.arity)
	))
	| _       , C0       ->
	(Utils.M3Edge(if block1.neg
		then edge0
		else (make_edge_C0 false block0.arity)
	))
	| Id x0   , Id x1    ->
	(Utils.M3Edge(if x0 = x1
		then if block0.neg = block1.neg
			then edge0
			else (make_edge_C0 false block0.arity)
		else
		(
			let liste = MyList.init block0.arity (fun i ->
				if i = x0
					then X(block0.neg, 0)
				else if i = x1
					then X(block1.neg, 0)
					else P
			) in
			let tag = {hasS = false; hasP = block0.arity>2; maxX = Some 0} in
			let spx = (true, tag, liste) in
			({neg = true; arity = block0.arity; block = SPX spx}, Utils.Leaf())
		)
	))
	| Id x   , _         -> solve_and_id_pedge block0.arity block0.neg x edge1
	| _      , Id x      -> solve_and_id_pedge block0.arity block1.neg x edge0
	| SPX spx0, SPX spx1 -> if spx0 = spx1 && pnode0 = pnode1
		then (Utils.M3Edge (if block0.neg = block1.neg
			then edge0
			else (make_edge_C0 false block0.arity)))
		else (ifspx2 block0.arity block0.neg block1.neg (spx0, pnode0) (spx1, pnode1))


exception Return_False

let factoPP arity neg0 neg1 ((shift0, tag0, liste0), pnode0) ((shift1, tag1, liste1), pnode1) =
	let cfun = function (P, P) -> P, None | (x, y) -> (S, Some(x, y)) in
	let listeC, (liste0, liste1) = GUtils.consensus cfun liste0 liste1 in
	let arity' = List.length liste0 in
	assert(arity' = List.length liste1);
	let blockC = reduce_block_spx false arity  false                    false  listeC
	and block0 = reduce_block_spx neg0  arity' (Utils.gnode_is_leaf pnode0) shift0 liste0
	and block1 = reduce_block_spx neg1  arity' (Utils.gnode_is_leaf pnode1) shift1 liste1 in
	(blockC, ((block0, block1), pnode0, pnode1))

let final_and_ifspx2 arity neg0 neg1 spx_pnode0 spx_pnode1 = Utils.M3Node(factoPP arity neg0 neg1 spx_pnode0 spx_pnode1)

let rec facto_and_ifspx2 arity neg0 neg1 ((((shift0, tag0, liste0) as spx0), pnode0) as pedge0) ((((shift1, tag1, liste1) as spx1), pnode1) as pedge1) =
	if ddl_and then (
		print_string "facto: ";
		CpxDL.dummydump_edge ({neg = neg0; arity; block = SPX spx0}, pnode0) |> print_string;
		print_string " && ";
		CpxDL.dummydump_edge ({neg = neg1; arity; block = SPX spx1}, pnode1) |> print_string;
		print_newline()
	);
	let factoPP () = Utils.M3Node(factoPP arity neg0 neg1 pedge0 pedge1) in
	let extractXO liste =
		let listeC_peval, oplistec = List.map (function
			| X(b, 0) -> (X(b, 0), (Some(not b))), None           
			| X(b, i) -> (S      , None         ), Some(X(b, i-1)) 
			| e       -> (S      , None         ), Some e          
		) liste |> List.split in
		let listeC, peval = List.split listeC_peval in
		let listec = MyList.unop oplistec in
		(listeC, peval, listec)
	in
	let extractX0X0 listeX listeY =
		let cfun = function
			| P       , P        -> P, (None, None)
			| X(bX, 0), X(bY, 0) ->
			(if bX = bY
				then (X(bX, 0), (None, None))
				else (raise Return_False)
			)
			| X(bX, 0), e        -> X(bX, 0), (None, Some(Some(not bX), e))
			| e       , X(bY, 0) -> X(bY, 0), (Some(Some(not bY), e), None)
			| eX      , eY       -> S       , (Some(None, eX), Some(None, eY))
		in
		let listeC, pevalX_listeX, pevalY_listeY = GUtils.consensus2 cfun listeX listeY in
		let pevalX, listeX = List.split pevalX_listeX
		and pevalY, listeY = List.split pevalY_listeY in
		(listeC, (pevalX, listeX), (pevalY, listeY))
	in
	let extractX1X1 listeX listeY =
		let cfun = function
			| P, P -> P, None
			| X(b, 0), X(b', 0) when b = b' -> X(b, 0), None
			| ex, ey -> S, (Some (ex, ey))
		in
		GUtils.consensus cfun listeX listeY
	in
	let factoXP () =
		let listeC, peval, liste0 = extractXO liste0 in
		let blockC = reduce_block_spx false arity false false listeC in
		let pedge0 = spx_liste_to_edge neg0 (not shift0) (liste0, pnode0) in
		let pedge1 = assign_pedge (Some peval) ({neg = neg1; arity; block = SPX spx1}, pnode1) in
		assert(arity_edge pedge0 = arity_edge pedge1);
		assert(count_nS_block blockC = arity_edge pedge0);	
		compose_utils_merge3 blockC (meta_solve_and pedge0 pedge1 facto_and_ifspx2)
	in
	let factoPX () =
		let listeC, peval, liste1 = extractXO liste1 in
		let blockC = reduce_block_spx false arity false false listeC in
		let pedge1 = spx_liste_to_edge neg1 (not shift1) (liste1, pnode1) in
		let pedge0 = assign_pedge (Some peval) ({neg = neg0; arity; block = SPX spx0}, pnode0) in
		assert(arity_edge pedge1 = arity_edge pedge0);
		assert(count_nS_block blockC = arity_edge pedge1);	
		compose_utils_merge3 blockC (meta_solve_and pedge1 pedge0 facto_and_ifspx2)
	in
	match tag0.maxX, tag1.maxX with
	| None      , None       -> (factoPP())
	| Some maxX0, None       -> (if neg0 <> shift0 then (factoPP()) else (factoXP ()))
	| None      , Some maxX1 -> (if neg1 <> shift1 then (factoPP()) else (factoPX ()))
	| Some maxX0, Some maxX1 ->
	(match neg0<>shift0, neg1<>shift1 with
		| false, false ->
		(
			try
			(
				let listeC, (peval0, liste0), (peval1, liste1) = extractX0X0 liste0 liste1 in
				let blockC = reduce_block_spx false arity false false listeC in
				let pedge0 = spx_liste_to_edge neg0 shift0 (liste0, pnode0) |> assign_pedge (Some peval0)
				and pedge1 = spx_liste_to_edge neg1 shift1 (liste1, pnode1) |> assign_pedge (Some peval1) in
				assert(check_block blockC false);
				assert(check_edge pedge0);
				assert(check_edge pedge1);
				assert(arity_edge pedge0 = arity_edge pedge1);
				assert(arity_edge pedge0 = count_nS_block blockC);
				let merge3 = meta_solve_and pedge0 pedge1 facto_and_ifspx2 in
				assert(check_utils_merge3 merge3);
				let merge3 = compose_utils_merge3 blockC merge3 in
				assert(check_utils_merge3 merge3);
				merge3
			)
			with Return_False -> Utils.M3Edge(make_edge_C0 false arity)
		)
		| true , false -> (factoPX ())
		| false, true  -> (factoXP ())
		| true , true  ->
		(
			let listeC, (liste0, liste1) = extractX1X1 liste0 liste1 in
			let blockC = reduce_block_spx false arity false true listeC in
			match liste0, liste1 with
			| (X(b, 0))::liste0, (X(b', 0))::liste1 ->
			(
				assert(b<>b');
				let pedge0 = spx_liste_to_edge neg0 shift0 (liste0, pnode0)
				and pedge1 = spx_liste_to_edge neg1 shift1 (liste1, pnode1) in
				Utils.M3Cons(blockC, (Tools.cswap b (pedge1, pedge0)))
			)
			| _ ->
			(
				let (block0, pnode0) as pedge0 = spx_liste_to_edge neg0 shift0 (liste0, pnode0)
				and (block1, pnode1) as pedge1 = spx_liste_to_edge neg1 shift1 (liste1, pnode1) in
				if get_maxX_block blockC = None
				then (Utils.M3Node (blockC, ((block0, block1), pnode0, pnode1)))
				else (compose_utils_merge3 blockC (meta_solve_and pedge0 pedge1 facto_and_ifspx2))
			)
		) 
	)

let solve_xor_id_spx_no_merge x shift liste =
	let rec aux carryC carryc i = function
		| [] -> (List.rev carryC, List.rev carryc)
		| head::tail -> match head with
			| P when i<>x  -> (aux (P::carryC)        carryc  (i+1) tail)
			| _            -> (aux (S::carryC) (head::carryc) (i+1) tail)
	in
	let listeC, listec = aux [] [] 0 liste in
	let tagC = get_spx_tag_from_block_spx listeC
	and tagc = get_spx_tag_from_block_spx listec in
	let rec aux n i = function
		| [] -> assert false
		| head::tail -> if i = x then n else match head with
			| P -> aux  n    (i+1) tail
			| _ -> aux (n+1) (i+1) tail
	in
	(false, tagC, listeC), (List.length listec), (aux 0 0 liste), (shift, tagc, listec)

let solve_xor_id_spx arity neg0 x0 neg1 (shift1, liste1, pnode1) =
	match List.nth liste1 x0 with
	| X(b1, 0) ->
	(
		let neg = neg0 <> neg1 <> b1 <> true in
		let func i = function
			| X(b, j) -> X(b, if i = x0 then (assert(j=0); 0) else j+1)
			| e -> e
		in
		let liste = List.mapi func liste1 in
		Utils.M3Edge(spx_liste_to_edge neg (not shift1) (liste, pnode1))
	)
	| _ ->
	(
		let spxC, (arityc:int), (xc:int), spxc = solve_xor_id_spx_no_merge x0 shift1 liste1 in
		if xc = 0
		then
		(
			let pedgec = ({neg = false; arity = arityc; block = SPX spxc}, pnode1) in
			assert(arity>=1);
			let tail = MyList.ntimes None (arityc-1) in
			let pedge0 =           assign_pedge (Some((Some false)::tail)) pedgec
			and pedge1 = neg_edge (assign_pedge (Some((Some true )::tail)) pedgec) in
			let blockC = {neg = neg0<>neg1; arity = arity; block = SPX spxC} in
			Utils.M3Cons(blockC, (pedge0, pedge1))
		)
		else
		(Utils.M3Node (
			{neg = neg0<>neg1; arity = arity; block = SPX spxC},
			((
				{neg = false; arity = arityc; block = Id xc},
				{neg = false; arity = arityc; block = SPX spxc}
			), Utils.Leaf(), pnode1)
		))
	)

let meta_solve_xor ((block0, pnode0) as edge0) ((block1, pnode1) as edge1) ifspx2 =
	if ddl_xor then (
		print_string "meta: ";
		CpxDL.dummydump_edge edge0 |> print_string;
		print_string " <> ";
		CpxDL.dummydump_edge edge1 |> print_string;
		print_newline()
	);
	assert(check_edge edge0);
	assert(check_edge edge1);
	assert(block0.arity = block1.arity);
	match block0.block, block1.block with
	| C0      , _        -> Utils.M3Edge(cneg_edge block0.neg edge1)
	| _       , C0       -> Utils.M3Edge(cneg_edge block1.neg edge0)
	| Id x    , Id y     ->
	(
		if x = y
		then (Utils.M3Edge(make_edge_C0 (block0.neg<>block1.neg) block0.arity))
		else
		(
			assert(pnode0 = Utils.Leaf());
			assert(pnode1 = Utils.Leaf());
			assert(block0.arity >= 2);
			let liste = MyList.init block0.arity (fun i -> if i = x || i = y then S else P) in
			let tag = {hasS = true; hasP = block0.arity>2; maxX = None} in
			let blockC = {neg = block0.neg<>block1.neg; arity = block0.arity; block = SPX(false, tag, liste)} in
			let block0 = {neg = false; arity = 1; block = Id 0}
			and block1 = {neg = true ; arity = 1; block = Id 0} in
			Utils.M3Cons(blockC, ((block0, Utils.Leaf()), (block1, Utils.Leaf())))
		)
	)
	| Id x   , SPX(shift, _, liste) -> solve_xor_id_spx block0.arity block0.neg x block1.neg (shift, liste, pnode1)
	| SPX(shift, _, liste), Id x    -> solve_xor_id_spx block0.arity block1.neg x block0.neg (shift, liste, pnode0)
	| SPX spx0, SPX spx1 -> if spx0 = spx1 && pnode0 = pnode1
		then (Utils.M3Edge(make_edge_C0 (block0.neg<>block1.neg) block0.arity))
		else (ifspx2 block0.arity block0.neg block1.neg (spx0, pnode0) (spx1, pnode1))

let final_xor_ifspx2 arity neg0 neg1 spx_pnode0 spx_pnode1 =
	let (blockC, ((block0, block1), pnode0, pnode1)) = factoPP arity neg0 neg1 spx_pnode0 spx_pnode1 in
	let blockC = cneg_block (block0.neg<>block1.neg) blockC
	and block0 = {neg = false; arity = block0.arity; block = block0.block}
	and block1 = {neg = false; arity = block1.arity; block = block1.block} in
	Utils.M3Node(blockC, ((block0, block1), pnode0, pnode1))

let rec facto_xor_ifspx2 arity neg0 neg1 ((((shift0, tag0, liste0) as spx0), pnode0) as pedge0) ((((shift1, tag1, liste1) as spx1), pnode1) as pedge1) : (_, _, _) Utils.merge3 =
	if ddl_xor then (
		print_string "facto: ";
		CpxDL.dummydump_edge ({neg = neg0; arity; block = SPX spx0}, pnode0) |> print_string;
		print_string " <> ";
		CpxDL.dummydump_edge ({neg = neg1; arity; block = SPX spx1}, pnode1) |> print_string;
		print_newline()
	);
	match tag0.maxX, tag1.maxX with
	| Some maxX0, Some maxX1 ->
	(
		let opmin = List.fold_left (fun opmin -> function
			| X(b, i), X(b', i') -> if (b=b')&&(i=i')
				then opmin
				else (Tools.opmin (min i i') opmin)
			| X(_, i), _
			| _      , X(_, i) -> Tools.opmin i opmin
			| _                -> opmin) None (List.combine liste0 liste1)
		in
		let shift, cfun = match opmin with
			| None -> false, (function
				| (P, P) -> P, None
				| X((b, i) as bi), X((b', _) as bi')->
					(
						assert(bi = bi');
						X(b, 0), None;
					)
				| (x, y) -> S, Some(x, y)
			)
			| Some min ->
			(
				let remapX = function X(b, i) -> assert(i>=min); X(b, i-min) | e -> e in
				(mod2 min), (function
					| P, P -> P, None
					| X(b, i), X(b', i') when (b=b')&&(i=i')&&(i<=min) -> X(b, 0), None
					| (x, y) -> S, Some (remapX x, remapX y)
				)
			)
		in
		let listeC, (liste0, liste1) = GUtils.consensus cfun liste0 liste1 in
		let blockC = reduce_block_spx (neg0<>neg1) arity false (shift0<>shift1) listeC in
		match liste0, liste1 with
		| (X(b, 0))::liste0, (X(b', 0))::liste1 ->
		(
			assert(b<>b');
			let pedge0 = spx_liste_to_edge (shift1<>shift) (shift0<>shift) (liste0, pnode0)
			and pedge1 = spx_liste_to_edge (shift0<>shift) (shift1<>shift) (liste1, pnode1) in
			Utils.M3Cons(blockC, Tools.cswap b (pedge1, pedge0))
		)
		| _ ->
		(
			let (block0, pnode0) as pedge0 = spx_liste_to_edge false (shift0<>shift) (liste0, pnode0)
			and (block1, pnode1) as pedge1 = spx_liste_to_edge false (shift1<>shift) (liste1, pnode1) in
			if get_maxX_block blockC = None
			then (Utils.M3Node (blockC, ((block0, block1), pnode0, pnode1)))
			else (compose_utils_merge3 blockC (meta_solve_xor pedge0 pedge1 facto_xor_ifspx2))
		)
	)
	| _ -> (final_xor_ifspx2 arity neg0 neg1 pedge0 pedge1)

let meta_solve_binop solver pedge0 pedge1 : (_, _, _) Utils.merge3 =
	let reduce_assign = function
		| Some peval when List.exists (function Some _ -> true | _ -> false) peval -> Some peval
		| _ -> None
	in
	let reduce_pnode = function
		| Utils.Leaf () -> Utils.Leaf ()
		| Utils.Node (peval, node) -> Utils.Node(reduce_assign peval, node)
	in
	let reduce_pedge (block, pnode) = (block, reduce_pnode pnode) in
	assert(check_edge pedge0);
	assert(check_edge pedge1);
	assert(arity_edge pedge0 = arity_edge pedge1);
	match solver pedge0 pedge1 with
	| Utils.M3Edge pedge -> (assert(check_edge pedge); Utils.M3Edge pedge)
	| Utils.M3Cons (blockC, (pedge0, pedge1)) ->
	(
		assert(check_block blockC false);
		assert(check_edge pedge0);
		assert(check_edge pedge1);
		assert(arity_edge pedge0 = arity_edge pedge1);
		assert(arity_edge pedge0 + 1 = count_nS_block blockC);
		match compose_utils_merge blockC (solve_cons pedge0 pedge1) with
		| Utils.MEdge pedge -> Utils.M3Edge pedge
		| Utils.MNode (blockC, ((block0, block1), pnode0, pnode1)) -> Utils.M3Cons (blockC, ((block0, pnode0), (block1, pnode1)))
	)
	| Utils.M3Node (blockC, ((block0, block1), pnode0, pnode1)) ->
	(
		let pedge0 = reduce_pedge (block0, pnode0)
		and pedge1 = reduce_pedge (block1, pnode1) in
		assert(check_block blockC false);
		assert(check_edge pedge0);
		assert(check_edge pedge1);
		assert(block0.arity = block1.arity);
		assert(block0.arity = count_nS_block blockC);
		let pedge0, pedge1 = Tools.cswap (not(pedge0<=pedge1)) (pedge0, pedge1) in
		let block0, pnode0 = pedge0
		and block1, pnode1 = pedge1 in
		Utils.M3Node (blockC, ((block0, block1), pnode0, pnode1))
	)
	

let solve_and pedge0 pedge1 : (_, _, _) Utils.merge3 =
	if ddl_and then (print_string "@@solve_and:"; print_newline());
	meta_solve_binop (fun pedge0 pedge1 -> meta_solve_and pedge0 pedge1 facto_and_ifspx2) pedge0 pedge1

let solve_xor pedge0 pedge1 : (_, _, _) Utils.merge3 =
	if ddl_xor then (print_string "@@solve_xor:"; print_newline());
	meta_solve_binop (fun pedge0 pedge1 -> meta_solve_xor pedge0 pedge1 facto_xor_ifspx2) pedge0 pedge1
	
let node_push_ande _ (pedge0, pedge1) = match solve_and pedge0 pedge1 with
	| Utils.M3Edge edge -> Utils.M3Edge edge
	| Utils.M3Cons cons -> Utils.M3Cons cons
	| Utils.M3Node (block, (block2, pnode0, pnode1)) ->
		Utils.M3Node (block, (CpxDL.bindump_node block2, pnode0, pnode1))

let pnode_of_node = function
	| Utils.Leaf () -> Utils.Leaf ()
	| Utils.Node node -> Utils.Node (None, node)

let pedge_of_edge (block, node) = (block, pnode_of_node node)

let node_push_cons _ x y = match solve_cons x y with
	| Utils.MEdge edge -> Utils.MEdge edge
	| Utils.MNode (block, (block2, node0, node1)) -> Utils.MNode (block, (CpxDL.bindump_node block2, node0, node1))

let tacx_propa_cons _ x y =
	match solve_cons x y with
	| Utils.MEdge (block, pnode) ->
		Utils.MEdge(block, pnode)
	| Utils.MNode (block, ((block0, block1), pnode0, pnode1)) -> 
		Utils.MNode(block, (TacxTypes.Cons, (block0, pnode0), (block1, pnode1)))

let tacx_propa_and _ x y = match solve_and x y with
	| Utils.M3Edge edge -> Utils.MEdge edge
	| Utils.M3Cons (block, (pedge0, pedge1)) -> Utils.MNode (block, (TacxTypes.Cons, pedge0, pedge1))
	| Utils.M3Node (block, ((block0, block1), pnode0, pnode1)) -> Utils.MNode (block, (TacxTypes.And, (block0, pnode0), (block1, pnode1)))

let tacx_propa_xor _ x y = match solve_xor x y with
	| Utils.M3Edge edge -> Utils.MEdge edge
	| Utils.M3Cons (block, (pedge0, pedge1)) -> Utils.MNode (block, (TacxTypes.Cons, pedge0, pedge1))
	| Utils.M3Node (block, ((block0, block1), pnode0, pnode1)) -> Utils.MNode (block, (TacxTypes.Xor, (block0, pnode0), (block1, pnode1)))

let tacx_propa gid = TacxTypes.(function
	| Cons -> tacx_propa_cons gid
	| And  -> tacx_propa_and  gid
	| Xor  -> tacx_propa_xor  gid)

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

let eval_pedge eval pedge =
	let block, pnode = assign_pedge (Some(List.map(fun x -> Some x)eval)) pedge in
	if block.block = C0 then (assert(pnode = Utils.Leaf()); (Some block.neg)) else None

let eval_merge3 solve eval merge3 = match assign_merge3 solve (Some(List.map(fun x -> Some x)eval)) merge3 with
	| Utils.M3Edge (block, Utils.Leaf()) -> (if block.block = C0 then (Some block.neg) else None)
	| _ -> None

