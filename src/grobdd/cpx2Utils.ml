(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open Cpx2Types

let arity_block block = block.arity
let arity_edge (block, _) = arity_block block
let neg_block block = {neg = not block.neg; arity = block.arity; block = block.block}
let cneg_block neg block = {neg = neg <> block.neg; arity = block.arity; block = block.block}
let neg_edge (block, node) = (neg_block block, node)
let cneg_edge neg (block, node) = (cneg_block neg block, node)

let (=>) (x:bool) (y:bool) : bool = (x <= y)
(*
assert((false => false) = true )
assert((false => true ) = true )
assert((true  => false) = false)
assert((true  => true ) = true )
*)

let make_nP n = MyList.make n P
let make_nS n = MyList.make n S

let isS		= function S -> true		| _ -> false
let isntS = function S -> false		| _ -> true
let isP		= function P -> true		|	_ -> false
let isntP = function P -> false		| _ -> true
let isX		= function X _ -> true	| _ -> false
let isntX = function X _ -> false	| _ -> true


let hasS block = match block.block with
	| C0 | Id _ -> false
	| SPX (_, tag, _) -> tag.hasS

let block_split (blockX, blockY) : block * block = (blockX, blockY)

let block_merge blockX blockY = (blockX, blockY)

let node_split (block, iX, iY) =
	let blockX, blockY = block_split block in
	(blockX, iX), (blockY, iY)

let node_merge (blockX, iX) (blockY, iY) =
	(block_merge blockX blockY, iX, iY)

(* return true if x is odd, false otherwise *)
let mod2 x = (x mod 2) = 1

let block_arity block = block.arity
let edge_arity (block, _) = block_arity block

(* check that a block is properly formed *)
(* TODO
	- check for trailing 0s
	- check that shift is set to 0 if their is no X elem
	- check for a single trailing 1
	- check for sub-normal case +/- I (if false then true else false)
*)

let block_spx_is_const liste = List.for_all isP

let block_is_const block = match block.block with
	| C0 -> Some block.neg
	| _	 -> None

let edge_is_const (block, next) = match next with
	| Utils.Leaf() -> block_is_const block
	| _            -> None

let block_c0_to_block_spx arity =
	(false, {hasS = false; hasP = arity > 0; maxX = None}, make_nP arity )

let block_id_to_block_spx arity x =
		assert(arity > x);
		(true,  {hasS = false; hasP = arity > 1; maxX = Some 0}, (make_nP x)@((X(true, 0))::(make_nP (arity-x-1))))

let block_choice_to_block_spx arity = function
	| C0 -> block_c0_to_block_spx arity
	| Id x	-> block_id_to_block_spx arity x
	| SPX (shift, tag, liste) ->
		(shift, tag, liste)

let push_S_block block =
	let (shift, tag, liste) = block_choice_to_block_spx block.arity block.block in
	let tag = {hasS = true; hasP = tag.hasP; maxX = tag.maxX} in
	{neg = block.neg; arity = block.arity+1; block = SPX(shift, tag, S::liste)}

let push_S_edge (block, node) = (push_S_block block, node)

let push_P_block_choice = function
	| C0 -> C0
	| Id x -> Id (x+1)
	| SPX (shift, tag, liste) ->
		let tag = {hasS = tag.hasS; hasP = true; maxX = tag.maxX} in
		SPX (shift, tag, P::liste)

let push_P_block block = {neg = block.neg; arity = block.arity+1; block = push_P_block_choice block.block}

let push_P_edge (block, node) = (push_P_block block, node)

let minicheck_spx = List.for_all (function X(_, i) when i < 0 -> false | _ -> true)

let get_spx_tag_from_block_spx liste =
	let hasS, hasP, maxX = List.fold_left (fun (hasS, hasP, maxX) -> function
		| S			-> (true, hasP, maxX					)
		| P			-> (hasS, true, maxX					)
		| X(b, i)	-> (hasS, hasP, Tools.opmax i maxX	)
	) (false, false, None) liste in
	{hasS; hasP; maxX}

let check_spx_tag tag liste : bool =
	(if tag.hasS then (List.exists isS liste) else (List.for_all isntS liste))&&
	(if tag.hasP then (List.exists isP liste) else (List.for_all isntP liste))&&
	(match tag.maxX with
		| None -> List.for_all isntX liste
		| Some 0 ->
		(
			(List.for_all (function X(_, i) -> i = 0 | _ -> true) liste)&&
			(List.exists isX liste)
		)	
		| Some maxX ->
		(
			(List.for_all (function X(_, i) -> (0<=i)&&(i<=maxX) | _ -> true ) liste)&&
			(List.exists (function X(_, i) when i = maxX -> true | _ -> false) liste)
		)	
	)

let get_spx_tag arity = function
	| C0		-> {hasS = false; hasP = arity > 0; maxX = None}
	| Id _	-> {hasS = false; hasP = arity > 1; maxX = None}
	| SPX (_, tag, liste) ->
		assert(check_spx_tag tag liste);
		tag

let get_maxX_block block = match block.block with
	| C0 | Id _ -> None
	| SPX(_, tag, _) -> tag.maxX

(**	Determine (if possible) the value of the function according to its top descriptor [block] for the valuation [x]
		Returns [None] otherwise
 **)

let in_block_spx (shift, tag, liste) sigma =
	match tag.maxX with
	| None -> (if tag.hasS then None else Some false)
		(* if no S and no X, then its a constant *)
	| Some maxX ->
	(
		match List.fold_left (fun opmin -> function
			| X(b, i), b' when b = b' ->
				Tools.opmin i opmin
			| _ -> opmin ) None (List.combine liste sigma) with
		| None -> if tag.hasS then None else Some false
		| Some i -> Some(shift <> mod2 i)
	)

let in_block_choice block sigma = match block with
	| C0 -> Some false
	| Id x -> Some(List.nth sigma x)
	| SPX block_spx -> in_block_spx block_spx sigma

let in_block block sigma = match in_block_choice block.block sigma with
	| None -> None
	| Some bool -> Some (block.neg <> bool)

let spx_is_contig (_, tag, liste) next_is_leaf : bool = match tag.maxX with
	(* checks that Xs are contiguous and that maxX(if not None) is really maxX *)
	| None -> true
	| Some maxX ->
	(
		let cnt = Array.make (maxX+1) 0 in
		let clk x = cnt.(x) <- cnt.(x) + 1 in
		List.iter (function X (_, x) -> clk x | _ -> ()) liste;
		(Array.for_all (fun x -> x > 0) cnt)&&(not(next_is_leaf && (cnt.(maxX) = 1)))
		(* check that the last line is not reduced to a single element *)
	)

let assert_block_spx arity (shift, tag, liste) next_is_leaf  =
	assert(arity = List.length liste);
	assert(check_spx_tag tag liste);
	assert(next_is_leaf => (not tag.hasS));
	match tag.maxX with
		| None -> assert((not shift)&&(not next_is_leaf))
		| Some 0 ->
			assert(next_is_leaf => (shift && (MyList.count isX liste > 1)))
		| Some maxX ->
		assert(
			(0 < maxX) && (maxX < arity) &&
			(MyList.count isX liste >= maxX+1) &&
			(next_is_leaf => (((mod2 maxX) <> shift)))
		)
	;
	assert(spx_is_contig (shift, tag, liste) next_is_leaf);
	()

let check_block_spx arity (shift, tag, liste) next_is_leaf : bool =
	(arity = List.length liste)&&
	(check_spx_tag tag liste)&&
	(next_is_leaf => (not tag.hasS))&&
	(tag.maxX = None => (shift = false))&&
	(match tag.maxX with
		| None -> (not shift)&&(next_is_leaf => tag.hasS)
		| Some 0 ->
			(next_is_leaf => (shift && (MyList.count isX liste > 1)))
		| Some maxX ->
			(0 < maxX) && (maxX < arity) &&
			(MyList.count isX liste >= maxX+1) &&
			(next_is_leaf => (((mod2 maxX) <> shift)))
	)&&
	(spx_is_contig (shift, tag, liste) next_is_leaf)

let assert_block_choice arity next_is_leaf = function
	| C0 -> ()
	| Id x -> assert ((x >= 0) && (x < arity)); ()
	| SPX block_spx -> assert_block_spx arity block_spx next_is_leaf; ()

let check_block_choice arity next_is_leaf = function
	| C0 -> next_is_leaf
	| Id x -> next_is_leaf && (x >= 0) && (x < arity)
	| SPX block_spx -> check_block_spx arity block_spx next_is_leaf

let assert_block block next_is_leaf =
	assert(0 <= block.arity);
	assert_block_choice block.arity next_is_leaf block.block;
	()

let check_block block next_is_leaf =
	assert(0 <= block.arity);
	assert(check_block_choice block.arity next_is_leaf block.block);
	()

let check_block block next_is_leaf =
	(0 <= block.arity) &&
	(check_block_choice block.arity next_is_leaf block.block)

let check_edge (block, node) = check_block block (Utils.gnode_is_leaf node)


let reduce_block_spx neg arity next_is_leaf shift liste =
	assert(List.length liste = arity);
	assert(minicheck_spx liste); (* just checks that Xs are positive *)
	let tag = get_spx_tag_from_block_spx liste in
	assert(next_is_leaf => (not tag.hasS));
	match tag.maxX with
	| None ->
		{neg; arity; block = (if next_is_leaf then C0 else SPX(false, tag, liste))}
	| Some maxX ->
	(
		(* check for contiguity *)
		let cnt = Array.make (maxX+1) 0 in
		let clk x = cnt.(x) <- cnt.(x) + 1 in
		List.iter (function X (_, x) -> clk x | _ -> ()) liste;
		let (shift, tag, liste) = if (Array.for_all (fun x -> x > 0) cnt)
			then (shift, tag, liste)
			else
			(
				(* Xs are not contiguous *)
				let minX = Tools.array_index (fun x -> x > 0) cnt |> Tools.unop in
				let shift = shift <> (mod2 minX) in
				let rec aux carry opt curr =
					assert(curr >= opt);
					if curr <= maxX
					then if cnt.(curr) > 0
						then if mod2 (curr-opt) <> (mod2 minX)
							then (aux ((opt+1)::carry) (opt+1) (curr+1))
							else (aux (opt::carry) opt (curr+1))
						else (aux ((-1)::carry) opt (curr+1))
					else (carry |> List.rev |> Array.of_list)
				in
				let remap = aux (0::(MyList.make minX (-1))) 0 (minX+1) in
				assert(Array.length remap = maxX+1);
				let liste = List.map (function S -> S | P -> P | X(b, i) -> X(b, remap.(i))) liste in
				let maxX = Array.fold_left max (-1) remap in
				assert(maxX >= 0);
				assert(List.exists (function X(_, i) when i = maxX -> true | _ -> false) liste);
				(shift, {hasS = tag.hasS; hasP = tag.hasP; maxX = Some maxX}, liste)
			)
		in
		let maxX = Tools.unop tag.maxX in
		if next_is_leaf
		then
		(
			assert(tag.hasS = false);
			(* check for a then 0 else 0 last line *)
			match (if shift <> (mod2 maxX)
				then (Ok (shift, tag, liste))
				else if maxX = 0
					then (Error {neg; arity; block = C0})
					else
					(
						let liste = List.map (function X(_, i) when i = maxX -> P | x -> x) liste in
						Ok (shift, {hasS = false; hasP = true; maxX = Some (maxX-1)}, liste)
					)
				) with
			| Error block -> block
			| Ok (shift, tag, liste) ->
			(
				(* check that the last line is not reduced to a single element *)
				let maxX = Tools.unop tag.maxX in
				if MyList.count (function X(_, i) when i = maxX -> true | _ -> false) liste = 1
				then if maxX = 0
					then
					(
						let x, neg' = MyList.ifind (function X(b, j) -> assert(j=0); Some b | _ -> None) liste |> Tools.unop in
						{neg = (neg = neg'); arity; block = Id x}
					)
					else
					(
						let tag = {hasS = tag.hasS; hasP = tag.hasP; maxX = Some(maxX-1)} in
						let liste = List.map (function X(b, i) when i = maxX -> X(not b, i-1) | x -> x) liste in
						{neg = not neg; arity; block = SPX(not shift, tag, liste)}
					)
				else {neg; arity; block = (SPX(shift, tag, liste))}
			)
			
		)
		else	{neg; arity; block = (SPX(shift, tag, liste))}
	)

let reduce_block_choice neg arity next_is_leaf = function
	| C0 -> {neg; arity; block = C0}
	| Id x -> assert(0 <= x && x < arity); {neg; arity; block = Id x}
	| SPX (shift, _, liste) -> reduce_block_spx neg arity next_is_leaf shift liste

let reduce_block block next_is_leaf =
	assert(0 <= block.arity);
	reduce_block_choice block.neg block.arity next_is_leaf block.block

let reduce_edge (block, node) =
	(reduce_block block (Utils.gnode_is_leaf node), node)

let spx_liste_to_block neg shift liste =
	reduce_block_spx neg (List.length liste) false shift liste

let spx_liste_to_edge neg shift (liste, node) =
	let block = reduce_block_spx neg (List.length liste) (Utils.gnode_is_leaf node) shift liste in
	(block, node)
	

let push_X0_block_spx iB tB (* if, rank = 0, then *) neg arity (shift, tag, liste) next_is_leaf =
	match tag.maxX with
	| None ->
	(
		assert(not next_is_leaf); (* next_is_leaf => no S; no S + no X => only P; C0 *)
		assert(not shift);
		let tag = {hasS = tag.hasS; hasP = tag.hasP; maxX = Some 0} in
		{neg; arity = arity+1; block = SPX(tB <> neg, tag, (X(iB, 0))::liste)}
	)
	| Some maxX ->
	(
		if (tB <> neg <> shift) = false
		(* correct alignment *)
		then {neg; arity = arity+1; block = SPX(shift, tag, (X(iB, 0))::liste)}
		(* incorrect alignment *)
		else
		(
			let liste = List.map (function X(b, i) -> X(b, i+1) | x -> x) liste in
			let tag = {hasS = tag.hasS; hasP = tag.hasP; maxX = Some(maxX+1)} in
			{neg; arity = arity+1; block = SPX(not shift, tag, (X(iB, 0))::liste)}
		)
	)

let push_X0_block_choice iB tB (* if, rank = 0, then *) neg arity next_is_leaf = function
	| C0 ->
	(
		assert(next_is_leaf);
		if neg = tB
		then {neg; arity = arity+1; block = C0}
		else {neg = iB <> tB; arity = arity+1; block = Id 0}
	)
	| Id x ->
	(
		assert(next_is_leaf);
		let tag = {hasS = false; hasP = arity>1; maxX = Some 0} in
		let tB' = tB <> neg in
		assert(arity >= x+1);
		let liste = (X(iB, 0))::((make_nP x)@((X(tB', 0))::(make_nP (arity-x-1)))) in
		{neg = not tB; arity = arity+1; block = SPX(true, tag, liste)}
	)
	| SPX block_spx -> push_X0_block_spx iB tB neg arity block_spx next_is_leaf

let push_X0_block iB tB (* if, rank = 0, then *) block next_is_leaf =
	push_X0_block_choice iB tB block.neg block.arity next_is_leaf block.block

let push_X0_edge iB tB (* if, rank = 0, then *) (block, node) =
	(push_X0_block iB tB block (Utils.gnode_is_leaf node), node)

let count_nS_spx = MyList.count isS

let count_nS_block_choice = function
	| C0 | Id _ -> 0
	| SPX(_, _, liste) -> count_nS_spx liste

let count_nS_block block = count_nS_block_choice block.block
let count_nS_edge (block, node) = count_nS_block block

let make_block_S neg arity =
	assert(arity>=0);
	{neg; arity; block = SPX(false, {hasS = arity > 0; hasP = false; maxX = None}, make_nS arity)}

let make_block_C0 neg arity =
	{neg; arity; block = C0}

let make_edge_C0 neg arity = (make_block_C0 neg arity, Utils.Leaf())
	

let make_block_P neg arity next_is_leaf =
	assert(arity>=0);
	{neg; arity; block = if next_is_leaf then C0 else SPX(false, {hasS = false; hasP = arity>0; maxX = None}, MyList.ntimes P arity)}

let make_block2_S (negX, negY) n =
	(make_block_S negX n, make_block_S negY n)

let cmake_nS  neg  block = make_block_S  neg  (count_nS_block block)
let cmake_nSS negs block = make_block2_S negs (count_nS_block block)


let tacx_split (ttag, block) =
	let blockX, blockY = block_split block in
	(ttag, blockX, blockY)

let compose_block_spx_spx (shift, tag, liste) (shift', tag', liste') =
	let compose shiftX spxC spxc =
		let rec aux carry = function
			| ([]   , []   ) -> List.rev carry
			| ([]   , _    ) -> assert false
			| (S::x', y::y') -> aux ((match y with X(b, i) -> X(b, shiftX+i) | y -> y)::carry) (x', y')
			| (S::_ , []	 ) -> assert false
			| (x::x', y'   ) -> aux (x::carry) (x', y')
		in
		aux [] (spxC, spxc)
	in
	let compose0 spxC spxc =
		let rec aux carry = function
			| ([]   , []   ) -> List.rev carry
			| ([]   , _    ) -> assert false
			| (S::x', y::y') -> aux (y::carry) (x', y')
			| (S::_ , []	 ) -> assert false
			| (x::x', y'   ) -> aux (x::carry) (x', y')
		in
		aux [] (spxC, spxc)
	in
	let shift, maxX, liste = match tag.maxX with
	| None ->
	(
		assert(shift = false);
		(shift', tag'.maxX, compose0 liste liste')
	)
	| Some maxX ->
	(
		match tag'.maxX with
		| None ->
		(
			assert(shift' = false);
			(shift, tag.maxX, compose0 liste liste')
		)
		| Some maxX' ->
		(
			let shiftX = if (shift <> (mod2 maxX)) = shift' then maxX else (maxX+1) in
			(shift, Some(shiftX+maxX'), compose shiftX liste liste')
		)
	)
	in
	let tag = {hasS = tag'.hasS; hasP = tag.hasP || tag'.hasP; maxX} in
	(shift, tag, liste)

let compose_block blockC blockc next_is_leaf =
	assert(check_block blockC false);
	assert(check_block blockc next_is_leaf);
	assert(count_nS_block blockC = blockc.arity);
	match blockC.block with
	| C0 | Id _ -> assert false
	| SPX(shift, tag, liste) ->
	let blockCc = match tag.maxX with
	| None ->
	(
		(* block SP *)
		(* no need for reduction *)
		assert(shift = false);
		match blockc.block with
		| C0 ->
		(
			assert(next_is_leaf);
			{neg = blockC.neg <> blockc.neg; arity = blockC.arity; block = C0}
		)
		| Id x ->
		(
			assert(next_is_leaf);
			(* blockC is SPX but only one S and no X *)
			let rec aux posC posc : _ list -> int = assert(posc>=0); function
					| [] -> assert false
					| head::tail -> match head with
						| S -> if posc=0
							then posC
							else (aux (posC+1) (posc-1) tail)
						| _ ->  aux (posC+1)  posc    tail
			in
			{neg = blockC.neg <> blockc.neg; arity = blockC.arity; block = Id (aux 0 x liste)}
		)
		| SPX(shift', tag', liste') -> 
		(
			let block_spx = compose_block_spx_spx (false, tag, liste) (shift', tag', liste') in
			{neg = blockC.neg <> blockc.neg; arity = blockC.arity; block = SPX block_spx}
		)
	)
	| Some maxX ->
	(
		(* needs reduction *)
		match blockc.block with
		| C0 ->
		(
			assert(next_is_leaf);
			let tag = {hasS = false; hasP = tag.hasP || tag.hasS; maxX = tag.maxX} in
			let liste = List.map (function S -> P |  e -> e) liste in
			{neg = blockC.neg <> blockc.neg; arity = blockC.arity; block = SPX(shift <> blockc.neg, tag, liste)}
		)
		| Id x ->
		(
			assert(next_is_leaf);
			let (shift', tag', liste') = block_id_to_block_spx blockc.arity x in
			let block_spx = compose_block_spx_spx (shift <> blockc.neg, tag, liste) (shift', tag', liste') in
			{neg = blockC.neg <> blockc.neg; arity = blockC.arity; block = SPX block_spx}
		)
		| SPX(shift', tag', liste') ->
		(
			let block_spx = compose_block_spx_spx (shift <> blockc.neg, tag, liste) (shift', tag', liste') in
			{neg = blockC.neg <> blockc.neg; arity = blockC.arity; block = SPX block_spx}
		)
	)
	in
	reduce_block blockCc next_is_leaf

let compose_edge blockC (blockc, nodec) = (compose_block blockC blockc (Utils.gnode_is_leaf nodec), nodec)

let compose_utils_merge blockC = function
	| Utils.MEdge edge -> Utils.MEdge(compose_edge blockC edge)
	| Utils.MNode (block, node) -> Utils.MNode(compose_block blockC block false, node)

let compose_utils_merge_node_edge block : (_, _) Utils.merge -> _ edge = function
	| Utils.MEdge edge -> compose_edge block edge
	| Utils.MNode node -> reduce_edge (block, Utils.Node node)

let compose_utils_merge3 blockC = function
	| Utils.M3Edge edge -> Utils.M3Edge(compose_edge blockC edge)
	| Utils.M3Cons (block, node) -> Utils.M3Cons(compose_block blockC block false, node)
	| Utils.M3Node (block, node) -> Utils.M3Node(compose_block blockC block false, node)

let arity_utils_merge = function
	| Utils.MEdge (block, _)
	| Utils.MNode (block, _) -> block.arity

let arity_utils_merge3 = function
	| Utils.M3Edge (block, _)
	| Utils.M3Cons (block, _)
	| Utils.M3Node (block, _) -> block.arity

let check_utils_mergeC = function
	| Utils.MEdge edge -> check_edge edge
	| Utils.MNode (block, ((block0, block1), node0, node1)) ->
	(
		(check_block block false)&&
		(check_edge (block0, node0))&&
		(check_edge (block1, node1))&&
		(block0.arity = block1.arity)&&
		(count_nS_block block = block0.arity + 1)
	)

let check_utils_mergeAX = function
	| Utils.MEdge edge -> check_edge edge
	| Utils.MNode (block, ((block0, block1), node0, node1)) ->
	(
		(check_block block false)&&
		(check_edge (block0, node0))&&
		(check_edge (block1, node1))&&
		(block0.arity = block1.arity)&&
		(count_nS_block block = block0.arity)
	)

let check_utils_merge3 = function
	| Utils.M3Edge edge -> check_edge edge
	| Utils.M3Cons (block, (edge0, edge1)) ->
	(
		(check_block block false)&&
		(check_edge edge0)&&
		(check_edge edge1)&&
		(arity_edge edge0 = arity_edge edge1)&&
		(count_nS_block block = arity_edge edge0 + 1)
	)
	| Utils.M3Node (block, ((block0, block1), node0, node1)) ->
	(
		(check_block block false)&&
		(check_edge (block0, node0))&&
		(check_edge (block1, node1))&&
		(block0.arity = block1.arity)&&
		(count_nS_block block = block0.arity)
	)

let get_root b (block, _) = ({neg = b; arity = block.arity; block = C0}, Utils.Leaf())

let neg (block, node) = ({neg = not block.neg; arity = block.arity; block = block.block}, node)
let cneg b (block, node) = ({neg = b <> block.neg; arity = block.arity; block = block.block}, node)

let assign_edge' set : _ edge -> _ edge * peval =
	let arity = List.length set in
	let arity' = MyList.count (function None -> true | _ -> false) set in
	fun (block, node) ->
		assert(check_edge (block, node));
		assert(arity = block.arity);
		match block.block with
		| C0 -> (({neg = block.neg; arity = arity'; block = C0}, Utils.Leaf()), None)
		| Id x ->
		(
			match List.nth set x with
			| Some b -> (({neg = block.neg <> b; arity = arity'; block = C0}, Utils.Leaf()), None)
			| None ->
			(
				let x' = MyList.counti (fun i -> function None when i < x -> true | _ -> false) set in
				(({neg = block.neg; arity = arity'; block = Id x'}, Utils.Leaf()), None)
			)
		)
		| SPX(shift, tag, liste) ->
		(
			let opsubopset, opmin = MyList.foldmap (fun (opmin : int option) -> fun ((set : bool option), elem) -> match set with
				| None      -> ((Some elem, (match elem with S -> Some None | _ -> None)), opmin)
				| Some set  -> match elem with
					| P		    -> ((None     , None	                                      ), opmin)
					| S		    -> ((None     , Some (Some set)                             ), opmin)
					| X(b, i) -> let opmin = if b = set then Tools.opmin i opmin else opmin in
											 ((None     , None                                        ), opmin)
				) (fun _ -> true) None (List.combine set liste) in
			let opliste, opset = List.split opsubopset in
			let liste = MyList.list_of_oplist opliste
			and set   = MyList.list_of_oplist opset   in
			match opmin with
			| None ->
			(
				let block = reduce_block_spx block.neg arity' (Utils.gnode_is_leaf node) shift liste in
				((block, node), (if List.for_all ((=)None) set then None else (Some set)))
			)
			| Some min ->
			(
				let liste = List.map (function X(_, i) as e when i < min -> e | _ -> P) liste in
				let neg = block.neg <> shift <> (mod2 min) in
				let block = reduce_block_spx neg arity' true (mod2 min) liste in
				((block, Utils.Leaf()), None)
			)
		)

let assign_edge = function
	| None -> (fun edge -> (edge, None))
	| Some set -> (fun edge -> assign_edge' set edge)

let check_peval = function
	| None -> true
	| Some peval -> List.exists (function Some _ -> true | None -> false) peval

let compose_peval pevalC pevalc =
	(* WARNING : arity(pevalC) <= arity(pevalc) *)
	match pevalC, pevalc with
	| None, None               -> None
	| Some peval, None
	| None, Some peval         -> Some peval
	| Some pevalC, Some pevalc ->
	(
		let rec aux carry = function
			| ([]   , []       ) -> List.rev carry
			| (_    , []       ) 
			| ([]   , None:: _ ) -> assert false
			| (x::x', None::y' ) -> aux (x::carry) (x', y')
			| (x'   , y::y'    ) -> aux (y::carry) (x', y')
		in Some(aux [] (pevalC, pevalc))
	)

let assign_pedge' peval (block, pnode) : _ pedge =
	let (block', pnode'), peval' = assign_edge' peval (block, pnode) in
	let pnode' = match pnode' with
		| Utils.Leaf () -> assert(peval' = None); Utils.Leaf()
		| Utils.Node (peval, node) -> Utils.Node(compose_peval peval' peval, node)
	in
	(block', pnode')

let assign_pedge peval (block, pnode) : _ pedge =
	let (block', pnode'), peval' = assign_edge peval (block, pnode) in
	let pnode' = match pnode' with
		| Utils.Leaf () -> assert(peval' = None); Utils.Leaf()
		| Utils.Node (peval, node) -> Utils.Node(compose_peval peval' peval, node)
	in
	(block', pnode')

let assign_dummydump = StrDump.(option(list(option bool)))

