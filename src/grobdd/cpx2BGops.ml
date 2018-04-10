(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open Utils
open Cpx2BTypes
open Cpx2BUtils

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
	let tagC = get_spx_tag_from_spx listeC
	and tagc = get_spx_tag_from_spx listec in
	let rec aux n i = function
		| [] -> assert false
		| head::tail -> if i = x then n else match head with
			| P -> aux  n    (i+1) tail
			| _ -> aux (n+1) (i+1) tail
	in
	(false, tagC, listeC), (List.length listec), (aux 0 0 liste), (shift, tagc, listec)
	

let meta_solve_cons' ((), ((block0, node0) as edge0), ((block1, node1) as edge1)) ifspx2 =
	if edge0 = edge1
	then (merge_of_edge(push_P_edge edge0))
	else match block0.block, block1.block with
	| C0      , _        -> merge_of_edge(push_X0_edge false block0.neg edge1)
	| _       , C0       -> merge_of_edge(push_X0_edge true  block1.neg edge0)
	| Id x0   , Id x1    ->
	(
		if x0 = x1
		then
		(
			assert(block0.neg <> block1.neg);
			let liste = S::(MyList.init block0.arity (fun i -> if i = x0 then S else P)) in
			let tag = {hasS = true; hasP = block0.arity > 1; maxX = None} in
			let block_spx = (false, tag, liste) in
			(
				{neg = block0.neg; arity = block0.arity+1; block = SPX block_spx},
				MNode ((),
					({neg = false; arity = 1; block = Id 0}, node0),
					({neg = true ; arity = 1; block = Id 0}, node1)
				)
			)
		)
		else
		(
			let liste = S::(MyList.init block0.arity (fun i -> if i = x0 || i = x1 then S else P)) in
			let id0, id1 = if x0 < x1 then (0, 1) else (1, 0) in
			let tag = {hasS = true; hasP = block0.arity > 2; maxX = None} in
			let block_spx = (false, tag, liste) in
			(
				{neg = block0.neg; arity = block0.arity+1; block = SPX block_spx},
				MNode ((),
					({neg = false; arity = 2; block = Id id0}, node0),
					({neg = block0.neg <> block1.neg; arity = 2; block = Id id1}, node1)
				)
			)
		)
	)
	| Id x   , SPX (shift, _, liste) ->
	(
		match List.nth liste x with
		| X(b, 0) when (b <> block0.neg) = (shift <> block1.neg) ->
		(
			let liste' = (X(false, 1))::(List.mapi (fun y -> function X(b, i) when x<>y -> X(b, i+2) | e -> e) liste) in
			let block = reduce_block_spx block1.neg (block1.arity+1) (gnode_is_leaf node1) shift liste' in
			(block, MEdge node1)
		)
		| _ ->
		(
			let spxC, (arityc:int), (xc:int), spxc = solve_cons_id_spx_no_merge x shift liste in
			(
				{neg = block0.neg; arity = block0.arity+1; block = SPX spxC},
				MNode ((),
					({neg = false; arity = arityc; block = Id xc}, node0),
					({neg = block0.neg <> block1.neg; arity = arityc; block = SPX spxc}, node1)
				)
			)
		)
	)
	| SPX (shift, _, liste), Id x    ->
	(
		match List.nth liste x with
		| X(b, 0) when (shift <> block0.neg) = (b <> block1.neg) ->
		(
			let liste' = (X(true, 1))::(List.mapi (fun y -> function X(b, i) when x<>y -> X(b, i+2) | e -> e) liste) in
			let block = reduce_block_spx block0.neg (block0.arity+1) (gnode_is_leaf node0) shift liste' in
			(block, MEdge node0)
		)
		| _ ->
		(
			let spxC, arityc, xc, spxc = solve_cons_id_spx_no_merge x shift liste in
			(
				{neg = block0.neg; arity = block0.arity+1; block = SPX spxC},
				MNode ((),
					({neg = false; arity = arityc; block = SPX spxc}, node0),
					({neg = block0.neg <> block1.neg; arity = arityc; block = Id xc}, node1)
				)
			)
		)
	)
	| SPX spx0, SPX spx1 -> ifspx2 block0.arity block0.neg block1.neg (spx0, node0) (spx1, node1)


let meta_solve_cons (((), edge0, edge1) : 'i node) ifspx2 : 'i emerge =
	assert(check_edge edge0);
	assert(check_edge edge1);
	assert(arity_edge edge0 = arity_edge edge1);
	let blockC, merge = meta_solve_cons' ((), edge0, edge1) ifspx2 in
	assert(arity_edge edge0 + 1 = arity_block blockC);
	(match merge with
	| MEdge next ->
	(
		(match next with
		| Leaf () -> assert(snd edge0 = Leaf () && snd edge1 = Leaf ())
		| node -> match (snd edge0), (snd edge1) with
			| node'  , Leaf () 
			| Leaf (), node'   -> assert(node = node')
			| node0  , node1   -> assert(node = node0 && node = node1));
		assert(check_edge (blockC, next));
		()
	)
	| MNode ((), edge0', edge1') ->
	(
		assert(snd edge0 = snd edge0');
		assert(snd edge1 = snd edge1');
		assert(arity_edge edge0' = arity_edge edge1');
		assert(((fst edge0).block <> C0) && ((fst edge1).block <> C0));
		assert(check_block blockC false);
		assert(check_edge edge0');
		assert(check_edge edge1');
		()
	));
	(blockC, merge)


let final_cons_ifspx2 arity neg0 neg1 (spx0, i0) (spx1, i1) =
	(
		make_block_S neg0 (arity+1),
		MNode ((),
			({neg = false     ; arity; block = SPX spx0}, i0),
			({neg = neg0<>neg1; arity; block = SPX spx1}, i1)
		)
	)

let find_consensus_block_spx spxX spxY =
	List.fold_left2 (fun opmin x y -> match x, y with
			| X bi, X bi' -> if bi = bi'
				then opmin
				else (Tools.opmin (min (snd bi) (snd bi')) opmin)
			| X(_, i), _
			| _      , X(_, i) -> Tools.opmin i opmin
			| _                -> opmin
	) None spxX spxY
	

let facto_cons_ifspx2 arity neg0 neg1 ((s0, t0, l0), i0) ((s1, t1, l1), i1) =
	match t0.maxX, t1.maxX with
	| Some max0, Some max1 when (neg0 <> s0) = (neg1 <> s1) ->
	(
		let opmin = find_consensus_block_spx l0 l1 in
		let shift, cfun = match opmin with
			| None     -> false     , (function
				| P    , P     ->                   (P   , None      )
				| X bi , X bi' -> assert(bi = bi'); (X bi, None      )
				| x    , y     ->                   (S   , Some(x, y)))
			| Some min -> let remapX = shiftX (-min) in
										(mod2 min), (function
				| P    , P     ->                   (P   , None      )
				| X bi , X bi' when bi = bi' && snd bi <= min ->
																						(X bi, None      )
				| x    , y     ->										(S   , Some (remapX x, remapX y)))
		in
		let listeC, (listec0, listec1) = GUtils.consensus cfun l0 l1 in
		let blockC = reduce_block_spx neg0 (arity+1) false s0 (S::listeC)
		and edge0 = spx_liste_to_edge false        (s0<>shift) (listec0, i0)
		and edge1 = spx_liste_to_edge (neg0<>neg1) (s1<>shift) (listec1, i1) in
		let merge = meta_solve_cons ((), edge0, edge1) final_cons_ifspx2 in
		compose_merge blockC merge
	)
	| _ ->
	(
		let cfun = function (P, P) -> P, None | (x, y) -> (S, Some(x, y)) in
		let listeC, (listec0, listec1) = GUtils.consensus cfun l0 l1 in
		let blockC = reduce_block_spx neg0 (arity+1) false false (S::listeC)
		and edge0 = spx_liste_to_edge false        s0 (listec0, i0)
		and edge1 = spx_liste_to_edge (neg1<>neg0) s1 (listec1, i1) in
		(blockC, MNode((), edge0, edge1))
	)

let print_node head node = ()
(*	print_string head; print_string Cpx2BO3.(fst o3str_node' (conv_node node)); print_newline() *)

let solve_cons (((), edge0, edge1) as node) =
	print_node "$C-" node;
	let edge0 = reduce_edge edge0
	and edge1 = reduce_edge edge1 in
	let node = ((), edge0, edge1) in
	assert(check_edge edge0);
	assert(check_edge edge1);
	assert(arity_edge edge0 = arity_edge edge1);
	meta_solve_cons node facto_cons_ifspx2


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
		assert(node = Leaf());
		let edge' = ({neg = block.neg; arity = block.arity-1; block = C0}, Leaf()) in
		MEdge ((), edge', edge')
	)
	| Id 0 ->
	(
		assert(node = Leaf());
		let edge0 = ({neg = block.neg; arity = block.arity-1; block = C0}, Leaf())
		and edge1 = ({neg = not block.neg; arity = block.arity-1; block = C0}, Leaf()) in
		MEdge ((), edge0, edge1)
	)
	| Id x ->
	(
		assert(node = Leaf());
		assert(x > 0);
		let edge' = ({neg = block.neg; arity = block.arity-1; block = Id(x-1)}, Leaf()) in
		MEdge ((), edge', edge')
	)
	| SPX(shift, tag, liste) -> match liste with
		| [] -> assert false
		| elem::liste' ->
		(
			let block' = reduce_block_spx block.neg (block.arity-1) (gnode_is_leaf node) shift liste' in
			match elem with
			| P -> MEdge ((), (block', node), (block', node))
			| S -> MNode (gnode_node node, fun ((), edge0, edge1) ->
				((), compose_edge block' edge0, compose_edge block' edge1)
			)
			| X(b, i) ->
				let block_then, block_else = pull_X i block.neg block.arity shift liste' (gnode_is_leaf node) in
				let edge_then = (block_then, Leaf())
				and edge_else = (block_else, node) in
				let edge0, edge1 = Tools.cswap b (edge_then, edge_else) in
				MEdge ((), edge0, edge1)
		)

let solve_and_id_pedge arity neg0 x0 pedge1 =
	let peval = MyList.init arity (fun i -> if i = x0 then (Some(not neg0)) else None)
	and liste = MyList.init arity (fun i -> if i = x0 then (X(neg0, 0))     else S   ) in
	let tag = {hasP = false; hasS = arity>1; maxX = Some 0} in
	let blockC = {neg = false; arity; block = SPX (false, tag, liste)} in
	assert(check_block blockC false);
	assert(arity_edge pedge1 = arity);
	let pedge' = peval_pedge peval pedge1 in
	assert(arity_edge pedge' + 1 = arity);
	merge3_of_edge(compose_edge blockC pedge')


let meta_solve_and (((), ((block0, pnode0) as edge0), ((block1, pnode1) as edge1)) as pnode) ifspx2 =
	if ddl_and then (
		print_string "meta(&&): ";
		print_string (Cpx2BDump.pnode pnode);
		print_newline()
	);
(*	print_string "--@@meta_solve_and: begin"; print_newline();
	Cpx2BDump.pnode pnode |> print_string; print_newline(); *)
	assert(check_edge edge0);
	assert(check_edge edge1);
	assert(block0.arity = block1.arity);
	match block0.block, block1.block with
	| C0      , _        ->
	(merge3_of_edge(if block0.neg
		then edge1
		else (make_edge_C0 false block1.arity)
	))
	| _       , C0       ->
	(merge3_of_edge(if block1.neg
		then edge0
		else (make_edge_C0 false block0.arity)
	))
	| Id x0   , Id x1    ->
	(merge3_of_edge(if x0 = x1
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
			({neg = true; arity = block0.arity; block = SPX spx}, Leaf())
		)
	))
	| Id x   , _         -> solve_and_id_pedge block0.arity block0.neg x edge1
	| _      , Id x      -> solve_and_id_pedge block0.arity block1.neg x edge0
	| SPX spx0, SPX spx1 -> if spx0 = spx1 && pnode0 = pnode1
		then (merge3_of_edge(if block0.neg = block1.neg
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
	and edge0 = spx_liste_to_edge neg0 shift0 (liste0, pnode0)
	and edge1 = spx_liste_to_edge neg1 shift1 (liste1, pnode1) in
	(blockC, ((), edge0, edge1))

let final_and_ifspx2 arity neg0 neg1 spx_pnode0 spx_pnode1 =
	let edge, node = factoPP arity neg0 neg1 spx_pnode0 spx_pnode1 in
	(edge, M3Node node)

let rec facto_and_ifspx2 arity neg0 neg1 ((((shift0, tag0, liste0) as spx0), pnode0) as pedge0) ((((shift1, tag1, liste1) as spx1), pnode1) as pedge1) =
	if ddl_and then (
		print_string "facto: ";
		Cpx2BDump.edge ({neg = neg0; arity; block = SPX spx0}, pnode0) |> print_string;
		print_string " && ";
		Cpx2BDump.edge ({neg = neg1; arity; block = SPX spx1}, pnode1) |> print_string;
		print_newline()
	);
	let factoPP () =
		let edge, node = factoPP arity neg0 neg1 pedge0 pedge1 in
		(edge, M3Node node)
	in
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
		let pedge1 = peval_pedge peval ({neg = neg1; arity; block = SPX spx1}, pnode1) in
		let pnode = ((), pedge0, pedge1) in
		assert(arity_edge pedge0 = arity_edge pedge1);
		assert(count_nS_block blockC = arity_edge pedge0);
		if List.for_all isS listeC
		then (blockC, M3Node pnode)
		else (compose_merge3 blockC (meta_solve_and pnode facto_and_ifspx2))
	in
	let factoPX () =
		let listeC, peval, liste1 = extractXO liste1 in
		let blockC = reduce_block_spx false arity false false listeC in
		let pedge1 = spx_liste_to_edge neg1 (not shift1) (liste1, pnode1) in
		let pedge0 = peval_pedge peval ({neg = neg0; arity; block = SPX spx0}, pnode0) in
		let pnode = ((), pedge0, pedge1) in
		assert(arity_edge pedge1 = arity_edge pedge0);
		assert(count_nS_block blockC = arity_edge pedge1);	
		if List.for_all isS listeC
		then (blockC, M3Node pnode)
		else (compose_merge3 blockC (meta_solve_and pnode facto_and_ifspx2))
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
				let pedge0 = spx_liste_to_edge neg0 shift0 (liste0, pnode0) |> peval_pedge peval0
				and pedge1 = spx_liste_to_edge neg1 shift1 (liste1, pnode1) |> peval_pedge peval1 in
				assert(List.exists isntS listeC);
				compose_merge3 blockC (meta_solve_and ((), pedge0, pedge1) facto_and_ifspx2)
			)
			with Return_False -> merge3_of_edge(make_edge_C0 false arity)
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
				let pedge0, pedge1 = Tools.cswap (not b) (pedge0, pedge1) in
				(blockC, M3Cons ((), pedge0, pedge1))
			)
			| _ ->
			(
				let pedge0 = spx_liste_to_edge neg0 shift0 (liste0, pnode0)
				and pedge1 = spx_liste_to_edge neg1 shift1 (liste1, pnode1) in
				if get_maxX_block blockC = None
				then (blockC, (M3Node ((), pedge0, pedge1))) 
				else (compose_merge3 blockC (meta_solve_and ((), pedge0, pedge1) facto_and_ifspx2))
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
	let tagC = get_spx_tag_from_spx listeC
	and tagc = get_spx_tag_from_spx listec in
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
		merge3_of_edge(spx_liste_to_edge neg (not shift1) (liste, pnode1))
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
			let pedge0 =           peval_pedge ((Some false)::tail) pedgec
			and pedge1 = neg_edge (peval_pedge ((Some true )::tail) pedgec) in
			let blockC = {neg = neg0<>neg1; arity; block = SPX spxC} in
			(blockC, M3Cons((), pedge0, pedge1))
		)
		else
		(
			{neg = neg0<>neg1; arity = arity; block = SPX spxC},
			M3Node ((),
				({neg = false; arity = arityc; block = Id xc}, Leaf()),
				({neg = false; arity = arityc; block = SPX spxc}, pnode1)
			)
		)
	)

let meta_solve_xor ((), ((block0, pnode0) as edge0), ((block1, pnode1) as edge1)) ifspx2 =
	if ddl_xor then (
		print_string "meta: ";
		Cpx2BDump.edge edge0 |> print_string;
		print_string " <> ";
		Cpx2BDump.edge edge1 |> print_string;
		print_newline()
	);
	assert(check_edge edge0);
	assert(check_edge edge1);
	assert(block0.arity = block1.arity);
	match block0.block, block1.block with
	| C0      , _        -> (merge3_of_edge(cneg_edge block0.neg edge1))
	| _       , C0       -> (merge3_of_edge(cneg_edge block1.neg edge0))
	| Id x    , Id y     ->
	(
		if x = y
		then (merge3_of_edge(make_edge_C0 (block0.neg<>block1.neg) block0.arity))
		else
		(
			assert(pnode0 = Leaf());
			assert(pnode1 = Leaf());
			assert(block0.arity >= 2);
			let liste = MyList.init block0.arity (fun i -> if i = x || i = y then S else P) in
			let tag = {hasS = true; hasP = block0.arity>2; maxX = None} in
			let blockC = {neg = block0.neg<>block1.neg; arity = block0.arity; block = SPX(false, tag, liste)} in
			let edge0 = ({neg = false; arity = 1; block = Id 0}, Leaf())
			and edge1 = ({neg = true ; arity = 1; block = Id 0}, Leaf()) in
			(blockC, M3Cons((), edge0, edge1))
		)
	)
	| Id x   , SPX(shift, _, liste) -> solve_xor_id_spx block0.arity block0.neg x block1.neg (shift, liste, pnode1)
	| SPX(shift, _, liste), Id x    -> solve_xor_id_spx block0.arity block1.neg x block0.neg (shift, liste, pnode0)
	| SPX spx0, SPX spx1 -> if spx0 = spx1 && pnode0 = pnode1
		then (merge3_of_edge(make_edge_C0 (block0.neg<>block1.neg) block0.arity))
		else (ifspx2 block0.arity block0.neg block1.neg (spx0, pnode0) (spx1, pnode1))

let final_xor_ifspx2 arity neg0 neg1 spx_pnode0 spx_pnode1 =
	let blockC, (((), edge0, edge1) as node) = factoPP arity false false spx_pnode0 spx_pnode1 in
	assert((fst edge0).neg = false);
	assert((fst edge1).neg = false);
	let blockC = cneg_block (neg0<>neg1) blockC in
	(blockC, M3Node((), edge0, edge1))

let rec facto_xor_ifspx2 arity neg0 neg1 ((((shift0, tag0, liste0) as spx0), pnode0) as pedge0) ((((shift1, tag1, liste1) as spx1), pnode1) as pedge1) : _ plink emerge3 =
	if ddl_xor then (
		print_string "facto: ";
		Cpx2BDump.edge ({neg = neg0; arity; block = SPX spx0}, pnode0) |> print_string;
		print_string " <> ";
		Cpx2BDump.edge ({neg = neg1; arity; block = SPX spx1}, pnode1) |> print_string;
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
			let pedge0, pedge1 = Tools.cswap b (pedge1, pedge0) in
			(blockC, M3Cons ((), pedge0, pedge1))
		)
		| _ ->
		(
			let pedge0 = spx_liste_to_edge false (shift0<>shift) (liste0, pnode0)
			and pedge1 = spx_liste_to_edge false (shift1<>shift) (liste1, pnode1) in
			if get_maxX_block blockC = None
			then (blockC, M3Node ((), pedge0, pedge1))
			else (compose_merge3 blockC (meta_solve_xor ((), pedge0, pedge1) facto_xor_ifspx2))
		)
	)
	| _ -> (final_xor_ifspx2 arity neg0 neg1 pedge0 pedge1)

let meta_solve_binop (solver : 'i pnode -> 'i plink emerge3) ((((), pedge0, pedge1) as pnode) : 'i pnode) : 'i plink emerge3 =
	let reduce_pedge = GUtils.preduce_pedge in
	assert(check_edge pedge0);
	assert(check_edge pedge1);
	assert(arity_edge pedge0 = arity_edge pedge1);
	let blockC, merge = solver pnode in match merge with
	| M3Edge next -> merge3_of_edge (blockC, next)
	| M3Cons (((), pedge0', pedge1') as pnode') ->
	(
(*		print_string "--@@meta_solve_binop:M3Cons:begin"; print_newline();
		Cpx2BDump.block blockC |> print_string; print_newline();
		Cpx2BDump.pnode pnode' |> print_string; print_newline(); *)
		assert(check_block blockC false);
		assert(arity_edge pedge0 = arity_block blockC);
		assert(check_edge pedge0');
		assert(check_edge pedge1');
		assert(arity_edge pedge0' = arity_edge pedge1');
		assert(arity_edge pedge0' + 1 = count_nS_block blockC);
		let edge, merge as emerge = compose_merge blockC (solve_cons pnode') in
(*		Cpx2BDump.pemerge emerge |> print_string; print_newline();
		print_string "--@@meta_solve_binop:M3Cons:end"; print_newline(); *)
		(edge, match merge with MEdge next -> M3Edge next | MNode node -> M3Cons node)
	)
	| M3Node ((), pedge0', pedge1') ->
	(
		let pedge0' : 'i pedge = reduce_pedge pedge0'
		and pedge1' : 'i pedge = reduce_pedge pedge1' in
		assert(check_block blockC false);
		assert(arity_edge pedge0 = arity_block blockC);
		assert(check_edge pedge0');
		assert(check_edge pedge1');
		assert(arity_edge pedge0' = arity_edge pedge1');
		assert(arity_edge pedge0' = count_nS_block blockC);
		let pedge0', pedge1' = Tools.cswap (not(pedge0'<=pedge1')) (pedge0', pedge1') in
		(blockC, M3Node ((), pedge0', pedge1'))
	)
	

let solve_and (pnode : 'a pnode) =
	print_node "$A-" pnode;
(*	print_string "@@solve_and: begin"; print_newline();
	print_string (Cpx2BDump.pnode pnode); print_newline(); *)
	if ddl_and then (print_string "@@solve_and:"; print_newline());
	let return = meta_solve_binop (fun (pnode : 'a pnode) -> ((meta_solve_and pnode facto_and_ifspx2) : 'a plink emerge3)) (pnode : 'a pnode) in
	(*print_string (Cpx2BDump.pemerge3 return); print_newline();
	print_string "@@solve_and: end"; print_newline();*)
	return
	

let solve_xor pnode =
	print_node "$X-" pnode;
	if ddl_xor then (print_string "@@solve_xor:"; print_newline());
	meta_solve_binop (fun pnode -> meta_solve_xor pnode facto_xor_ifspx2) pnode
	
let pnode_of_node = function
	| Leaf () -> Leaf ()
	| Node node -> Node (None, node)

let pedge_of_edge (block, node) = (block, pnode_of_node node)

let tacx_cons node =
	let edge, merge = solve_cons node in
	(edge, match merge with
	| MEdge next -> MEdge next
	| MNode ((), edge0, edge1) -> MNode (TacxTypes.Cons, edge0, edge1))

let tacx_and  node =
	let edge, merge = solve_and  node in
	(edge, match merge with
	| M3Edge next -> MEdge next
	| M3Cons ((), edge0, edge1) -> MNode (TacxTypes.Cons, edge0, edge1)
	| M3Node ((), edge0, edge1) -> MNode (TacxTypes.And , edge0, edge1))

let tacx_xor  node =
	let edge, merge = solve_xor  node in
	(edge, match merge with
	| M3Edge next -> MEdge next
	| M3Cons ((), edge0, edge1) -> MNode (TacxTypes.Cons, edge0, edge1)
	| M3Node ((), edge0, edge1) -> MNode (TacxTypes.Xor , edge0, edge1))


let solve_of_tag = TacxTypes.(function
	| And  -> tacx_and
	| Cons -> tacx_cons
	| Xor  -> tacx_xor)

let solve_tacx (tag, edge0, edge1) =
	(solve_of_tag tag) ((), reduce_edge edge0, reduce_edge edge1)
let peval_merge3 (solve : 'i pnode -> 'i plink emerge3) (peval : peval) ((edge, merge) : 'i plink emerge3) : 'i plink emerge3 = match merge with
	| M3Edge pnext -> merge3_of_edge (peval_pedge peval (edge, pnext))
	| M3Cons (((), pedge0, pedge1) as pnode) ->
	(
		assert(arity_edge pedge0 = arity_edge pedge1);
		let (edge', pnext') = peval_pedge peval (edge, Node (None, ())) in
		match pnext' with
			| Leaf () -> (edge', (M3Edge(Leaf())))
			| Node (opeval, ()) -> match opeval with
				| None -> (edge', M3Cons pnode)
				| Some peval -> match peval_pnodeC peval pnode with
					| MEdge edge -> merge3_of_edge(compose_edge edge' edge)
					| MNode node ->
					(
						let edge', merge = compose_merge edge' (solve_cons node) in
						match merge with
						| MEdge next -> merge3_of_edge(edge', next)
						| MNode node -> (edge', M3Cons node)
					)
	)
	| M3Node (((), pedge0, pedge1) as pnode) ->
	(
		assert(arity_edge pedge0 = arity_edge pedge1);
		let edge', pnext' = peval_pedge peval (edge, Node (None, ())) in
		match pnext' with
			| Leaf () -> (edge', M3Edge(Leaf()))
			| Node (opeval, ()) ->
				compose_merge3 edge' (solve (opeval_pnode opeval pnode))
	)

let opquant_of_quant quant = if List.for_all (fun x -> x = false) quant
	then None
	else (Some quant)

let compose_quant qC qc = GUtils.compose false qc qC

let compose_opquant opqC opqc = match opqC, opqc with
	| None, None       -> None
	| Some q, None
	| None, Some q     -> Some q
	| Some qC, Some qc -> Some(compose_quant qC qc)

let compose_qnext quant = function
	| Utils.Leaf () -> assert(quant = []); Utils.Leaf()
	| Utils.Node (opq, node) -> Utils.Node(compose_opquant(opquant_of_quant quant)opq, node)

let quant_block_spx (quant : bool list) (neg : bool) (arity : int) ((shift, tag, liste) : block_spx) qnext =
	let opmin = List.fold_left2 (fun opmin -> fun quant elem -> match quant, elem with
		| true, X(_, i) when ((mod2 i)<>shift<>neg) = false -> Tools.opmin i opmin
		| _ -> opmin) None quant liste in
	match opmin with
	| None ->
	(
		let liste' = MyList.opmap2 (function
			| true  -> (fun _ -> None  )
			| false -> (fun x -> Some x)) quant liste in
		match qnext with
		| Utils.Leaf() -> spx_liste_to_edge neg shift (liste', Utils.Leaf())
		| Utils.Node (opq, node) ->
		(	
			let quant' = MyList.opmap2 (function
				| S -> (fun x -> Some x)
				| _ -> (fun _ -> None  )) liste quant in
			let opq' = compose_opquant(opquant_of_quant quant')opq in
			spx_liste_to_edge neg shift (liste', Utils.Node(opq', node))
		)
	)
	| Some minX ->
	(
		let liste' = MyList.opmap2 (function
			| true -> (fun _            -> None  )
			| false -> (function
				| S|P -> (Some P)
				| X(_, i) as x -> if i >= minX
					then   (Some P)
					else   (Some x))) quant liste in
		spx_liste_to_edge neg shift (liste', Utils.Leaf())
	)

let quant_block_choice (quant : bool list) (neg : bool) (arity : int) (qnext : (unit, bool list option * _) Utils.gnode) =
	let coarity = MyList.count(fun x -> x = false) quant in
	function
	| C0 -> make_edge_C0 neg coarity
	| Id x -> (if List.nth quant x
		then (make_edge_C0 neg coarity)
		else (
			let x' = MyList.counti (fun i b -> (b = false)&&(i < x)) quant in
			({neg; arity = coarity; block = Id x'}, Utils.Leaf())
		)
	)
	| SPX spx -> quant_block_spx quant neg arity spx qnext

let quant_qedge quant (block, qnext) = quant_block_choice quant block.neg block.arity qnext block.block
let quant_qnode  quant ((), edge0, edge1) =
	((), quant_qedge quant edge0, quant_qedge quant edge1)
let quant_node   quant node = quant_qnode quant (Utils.pnode_of_node node)


let solve_quant quant ((((), edge0, edge1) as qnode) : 'i qnode)  =
	assert(List.length quant = arity_edge edge0 + 1);
	assert(List.length quant = arity_edge edge1 + 1);
	assert(List.length quant > 0);
	if List.for_all (fun x -> x) quant
	then (Utils.M3Edge({neg = false; arity = 0; block = C0}, Utils.Leaf()))
	(* true in GroBdd due to canonicity, not necessarly true in TACX *)
	else ( match quant with
		| [] -> assert false
		| head::quant' -> if head
		then (Utils.M3Node(quant_qnode quant' qnode))
		else (Utils.M3Cons(quant_qnode quant' qnode))
	)
	(* =( cannot use solve_and in current version as we cannot propagate evaluation
	 * NEXT: introduce an EQUANT modele =)
	 * SOLUTION: cheating ... make solve_quant use evaluation (needs to performe a EQ inversion)
	 *)

let rewrite_expand_AX_node cond ((tag, edgeX, edgeY) as node) =
	let arity = arity_edge edgeX in
	assert(arity = arity_edge edgeY);
	let arity' = arity + TacxTypes.(match tag with Cons -> 1 | _ -> 0) in
	let blockC = make_block_S false arity' in
	if cond && (arity > 0) && (tag <> TacxTypes.Cons)
	then (
		let tail = MyList.ntimes None (arity-1) in
		let branch bool edgeX edgeY =
			let peval  = (Some bool)::tail in
			let edgeX' = peval_pedge peval edgeX
			and edgeY' = peval_pedge peval edgeY in
			let edge, merge = solve_tacx (tag, edgeX', edgeY') in
			(edge, (match merge with
			| Utils.MEdge next -> GTree.Leaf next
			| Utils.MNode node -> GTree.(Node(get_node_leaf node))))
		in
		let edge0 = branch false edgeX edgeY
		and edge1 = branch true  edgeX edgeY in
		(blockC, GTree.Node(TacxTypes.Cons, edge0, edge1))
	) else (blockC, GTree.(Node(get_node_leaf node)))
