(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open Utils
open Cpx2BTypes

let arity_block block = block.arity
let arity_edge (block, _) = arity_block block
let arity_node (tag, edge0, edge1) =
	assert(arity_edge edge0 = arity_edge edge1);
	arity_edge edge0 + TacxTypes.(match tag with Cons -> 1 | And | Xor -> 0)
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

let isS	  = function S   -> true	| _ -> false
let isntS = function S   -> false	| _ -> true
let isP	  = function P   -> true	| _ -> false
let isntP = function P   -> false	| _ -> true
let isX	  = function X _ -> true	| _ -> false
let isntX = function X _ -> false	| _ -> true

let shiftX shift = function X(b, i) -> X(b, i+shift) | elem -> elem

let hasS block = match block.block with
	| C0 | Id _ -> false
	| SPX (_, tag, _) -> tag.hasS

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

let spx_is_const liste = List.for_all isP liste

let block_is_const block = match block.block with
	| C0 -> Some block.neg
	| _	 -> None

let edge_is_const (block, next) = match next with
	| Leaf() -> block_is_const block
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

let get_spx_tag_from_spx liste =
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
	| C0	-> {hasS = false; hasP = arity > 0; maxX = None}
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
	(0 <= block.arity) &&
	(check_block_choice block.arity next_is_leaf block.block)

let check_edge (block, node) = check_block block (gnode_is_leaf node)

let reduce_block_spx neg arity next_is_leaf shift liste =
	assert(List.length liste = arity);
	assert(minicheck_spx liste); (* just checks that Xs are positive *)
	let tag = get_spx_tag_from_spx liste in
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
	(* print_string "@@reduce_block"; print_newline();
	print_string (Cpx2BDump.block block); print_newline();
	print_string ("next_is_leaf = "^(StrDump.bool next_is_leaf)); print_newline(); *)
	assert(0 <= block.arity);
	reduce_block_choice block.neg block.arity next_is_leaf block.block

let reduce_edge (block, node) =
	(reduce_block block (gnode_is_leaf node), node)

let spx_liste_to_block neg shift liste =
	reduce_block_spx neg (List.length liste) false shift liste

let spx_liste_to_edge neg shift (liste, node) =
	let block = reduce_block_spx neg (List.length liste) (gnode_is_leaf node) shift liste in
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
	(push_X0_block iB tB block (gnode_is_leaf node), node)

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

let make_edge_C0 neg arity = (make_block_C0 neg arity, Leaf())
	

let make_block_P neg arity next_is_leaf =
	assert(arity>=0);
	{neg; arity; block = if next_is_leaf then C0 else SPX(false, {hasS = false; hasP = arity>0; maxX = None}, MyList.ntimes P arity)}

let cmake_nS  neg  block = make_block_S  neg  (count_nS_block block)

let compose_block_spx_spx (shift, tag, liste) (shift', tag', liste') =
	(* print_string "@@compose_block_spx_spx: begin"; print_newline(); *)
	let compose0 = GUtils.compose S in
	let compose shift spxC spxc =
(*		print_string "--@@compose: begin"; print_newline();
		print_string ("List.length spxC = "^(StrDump.int(List.length spxC))); print_newline();
		print_string ("List.length spxc = "^(StrDump.int(List.length spxc))); print_newline(); *)
		let spxCc = compose0 spxC (List.map (shiftX shift) spxc) in
(*		print_string ("List.length spxCc = "^(StrDump.int(List.length spxCc))); print_newline();
		print_string "--@@compose: end"; print_newline(); *)
		spxCc
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
			(* print_string "-- Some maxX , Some maxX'"; print_newline(); *)
			let shiftX = if (shift <> (mod2 maxX)) = shift' then maxX else (maxX+1) in
			(shift, Some(shiftX+maxX'), compose shiftX liste liste')
		)
	)
	in
	let tag = {hasS = tag'.hasS; hasP = tag.hasP || tag'.hasP; maxX} in
	let return = (shift, tag, liste) in
	(*print_string (Cpx2BDump.block_spx return); print_newline();
	print_string "@@compose_block_spx_spx: end"; print_newline();*)
	return

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
(*
let compose_block blockC blockc next_is_leaf =
	(* pretty wrapper *)
	print_string "@@compose_block: begin"; print_newline();
	print_string ("blockC = "^(Cpx2BDump.block blockC)); print_newline();
	print_string ("blockC = "^(Cpx2BDump.block blockc)); print_newline();
	print_string ("next_is_leaf = "^(StrDump.bool next_is_leaf)); print_newline();
	let blockCc = compose_block blockC blockc next_is_leaf in
	print_string ("blockCc = "^(Cpx2BDump.block blockCc)); print_newline();
	print_string "@@compose_block: end"; print_newline();
	blockCc
*)
let compose_edge blockC (blockc, nodec) =
		(*print_string "@@compose_edge: start"; print_newline();*)
		let r = (compose_block blockC blockc (gnode_is_leaf nodec), nodec) in
		(*print_string "@@compose_edge: end"; print_newline();*)
		r


let compose_merge blockC (blockc, merge) = match merge with
	| MEdge next -> merge_of_edge (compose_edge blockC (blockc, next))
	| MNode node -> (compose_block blockC blockc false, MNode node)

let compose_merge_node_edge blockC = function
	| MEdge edge -> compose_edge blockC edge
	| MNode node -> (blockC, Node node)

let compose_merge3 blockC (blockc, merge) = match merge with
	| M3Edge next -> merge3_of_edge (compose_edge blockC (blockc, next))
	| _ -> (compose_block blockC blockc false, merge)

let arity_merge (block, _) = block.arity

let arity_merge3 (block, _) = block.arity

let check_mergeC (blockC, merge) = match merge with
	| MEdge  next -> check_edge (blockC, next)
	| MNode ((), edge0, edge1) ->
	(
		(check_block blockC false)&&
		(check_edge edge0)&&
		(check_edge edge1)&&
		(arity_edge edge0 = arity_edge edge1)&&
		(count_nS_block blockC = arity_edge edge0 + 1)
	)

let check_mergeAX (blockC, merge) = match merge with
	| MEdge  next -> check_edge (blockC, next)
	| MNode ((), edge0, edge1) ->
	(
		(check_block blockC false)&&
		(check_edge edge0)&&
		(check_edge edge1)&&
		(arity_edge edge0 = arity_edge edge1)&&
		(count_nS_block blockC = arity_edge edge0)
	)

let check_merge3 (blockC, merge) = match merge with
	| M3Edge  next -> check_edge (blockC, next)
	| M3Cons ((), edge0, edge1) ->
	(
		(check_block blockC false)&&
		(check_edge edge0)&&
		(check_edge edge1)&&
		(arity_edge edge0 = arity_edge edge1)&&
		(count_nS_block blockC = arity_edge edge0 + 1)
	)
	| M3Node ((), edge0, edge1) ->
	(
		(check_block blockC false)&&
		(check_edge edge0)&&
		(check_edge edge1)&&
		(arity_edge edge0 = arity_edge edge1)&&
		(count_nS_block blockC = arity_edge edge0)
	)

let get_root neg (block, _) = ({neg; arity = block.arity; block = C0}, Leaf())

let neg (block, node) = ({neg = not block.neg; arity = block.arity; block = block.block}, node)
let cneg b (block, node) = ({neg = b <> block.neg; arity = block.arity; block = block.block}, node)

let peval_edge (peval : peval) : 'i edge -> 'i edge * opeval =
	let arity = List.length peval in
	let arity' = MyList.count (function None -> true | _ -> false) peval in
	fun (block, node) ->
		assert(check_edge (block, node));
		assert(arity = block.arity);
		match block.block with
		| C0 -> (({neg = block.neg; arity = arity'; block = C0}, Leaf()), None)
		| Id x ->
		(
			match List.nth peval x with
			| Some b -> (({neg = block.neg <> b; arity = arity'; block = C0}, Leaf()), None)
			| None ->
			(
				let x' = MyList.counti (fun i -> function None when i < x -> true | _ -> false) peval in
				(({neg = block.neg; arity = arity'; block = Id x'}, Leaf()), None)
			)
		)
		| SPX(shift, tag, liste) ->
		(
			let opsuboppeval, opmin = MyList.foldmap (fun (opmin : int option) -> fun ((peval : bool option), elem) -> match peval with
				| None      -> ((Some elem, (match elem with S -> Some None | _ -> None)), opmin)
				| Some peval  -> match elem with
					| P		    -> ((None     , None	                                      ), opmin)
					| S		    -> ((None     , Some (Some peval)                           ), opmin)
					| X(b, i) -> let opmin = if b = peval then Tools.opmin i opmin else opmin in
											 ((None     , None                                        ), opmin)
				) (fun _ -> true) None (List.combine peval liste) in
			let opliste, oppeval = List.split opsuboppeval in
			let liste = MyList.list_of_oplist opliste
			and peval   = MyList.list_of_oplist oppeval   in
			match opmin with
			| None ->
			(
				let block = reduce_block_spx block.neg arity' (gnode_is_leaf node) shift liste in
				((block, node), (if List.for_all ((=)None) peval then None else (Some peval)))
			)
			| Some min ->
			(
				let liste = List.map (function X(_, i) as e when i < min -> e | _ -> P) liste in
				let neg = block.neg <> shift <> (mod2 min) in
				let block = reduce_block_spx neg arity' true (mod2 min) liste in
				((block, Leaf()), None)
			)
		)

let opeval_edge = function
	| None -> (fun edge -> (edge, None))
	| Some set -> (fun edge -> peval_edge set edge)

let check_peval peval = List.exists Tools.isSome peval

let check_opeval = function
	| None -> true
	| Some peval -> check_peval peval

let compose_peval = GUtils.compose_peval
let compose_opeval = GUtils.compose_opeval

let peval_pedge peval (block, pnode) : _ pedge =
	let (block', pnode'), peval' = peval_edge peval (block, pnode) in
	let pnode' = match pnode' with
		| Leaf () -> assert(peval' = None); Leaf()
		| Node (peval, node) -> Node(compose_opeval peval' peval, node)
	in
	(block', pnode')

let opeval_pedge = function
	| None -> (fun pedge -> pedge)
	| Some peval -> (fun pedge -> peval_pedge peval pedge)

let peval_pnode peval ((), pedge0, pedge1) =
	((), peval_pedge peval pedge0, peval_pedge peval pedge1)

let opeval_pnode = function
	| None -> (fun node -> node)
	| Some peval -> (fun node -> peval_pnode peval node)

let peval_pnodeC = function
	| [] -> assert false
	| None::peval -> (fun pnode -> MNode (peval_pnode peval pnode))
	| (Some false)::peval -> (fun ((), pedge0, _) -> MEdge (peval_pedge peval pedge0))
	| (Some true )::peval -> (fun ((), _, pedge1) -> MEdge (peval_pedge peval pedge1))

let select b ((), if0, if1) = if b then if1 else if0
