(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open CpxTypes
open CpxUtils

module CpxDL = CpxDumpLoad

let block_is_singleton block =
	let res = List.fold_left (function
		| None -> fun _ -> None
		| Some b -> (function
			| S -> None
			| P -> Some b
			| X _ -> if b then None else (Some true))
		) (Some false) block.sub in
	match res with
	| Some true -> true
	| _			-> false

let final_solve_cons blockC (blockX, iX) (blockY, iY) =
	let blockC = reduce blockC
	and blockX = reduce blockX
	and blockY = reduce blockY in
	let mnode () = Utils.MNode (
		reduce {
			neg = blockC.neg <> blockX.neg;
			shift = blockC.shift <> blockX.neg;
			sub = blockC.sub;
		},
		(
			{
				negX = false;
				negY = blockY.neg <> blockX.neg;
				shiftX = blockX.shift;
				shiftY = blockY.shift;
				subXY = List.combine blockX.sub blockY.sub;
			},
			iX,
			iY
		)
	) in
	match block_is_const blockX with
	| Some th -> Utils.MEdge(compose blockC (push_X false 0 th blockY, iY))
	| None -> match block_is_const blockY with
	| Some th -> Utils.MEdge(compose blockC (push_X true  0 th blockX, iX))
	| None ->
	(
		let allign = ( blockX.neg <> blockX.shift ) = ( blockY.neg <> blockY.shift ) in
		let xx0 () = List.fold_left (function
			| Some x -> (fun _ -> Some x)
			| None -> (function
				(* when ... && (if allign then b = b' else b <> b' ) *)
				| X(b, i), X(b', i') when i = 0 && i' = 0 && (allign <> b <> b') -> Some(b, b')
				| _ -> None)
			) None (List.combine blockX.sub blockY.sub)
		in
		match block_is_singleton blockX, block_is_singleton blockY with
		| true , true  ->
		(
			assert(blockX.shift = true);
			assert(blockY.shift = true);
			if blockX.sub = blockY.sub
			then
			(
				assert(List.length blockX.sub = 1);
				assert(List.length blockY.sub = 1);
				assert(blockX.neg <> blockY.neg);
				(* the first can't be non-significant without having been detected *)
			)
			else
			(
				assert(List.length blockX.sub = 2);
				assert(List.length blockY.sub = 2);
			);
			mnode()
		)
		| true , false ->
		(
			assert(blockX.shift = true);
			match xx0() with
			| None -> ( mnode() )
			| Some(_, b') ->
			(
				let subC', subY = GUtils.consensus0 (function
					| X _, X _ -> X(b', 0), None
					| _  , y   -> S, (Some y) ) blockX.sub blockY.sub in
				let negX = blockX.neg = allign in
				let blockY = reduce {
					neg = blockY.neg <> negX;
					shift = blockY.shift;
					sub = subY;
				} in
				let blockC' = reduce {
					neg = negX;
					shift = true;
					sub = S::subC';
				} in
				Utils.MEdge(compose blockC (compose blockC' (push_X false 0 false blockY, iY)))
			)
		)
		| false, true  ->
		(
			assert(blockY.shift = true);
			match xx0() with
			| None -> ( mnode() )
			| Some(b, _) ->
			(
				let subC', subX = GUtils.consensus0 (function
					| X _, X _ -> X(b, 0), None
					| x  , _   -> S, (Some x) ) blockX.sub blockY.sub in
				let negY = blockY.neg = allign in
				let blockX = reduce {
					neg = blockX.neg <> negY;
					shift = blockX.shift;
					sub = subX;
				} in
				let blockC' = reduce {
					neg = negY;
					shift = true;
					sub = S::subC';
				} in
				Utils.MEdge(compose blockC (compose blockC' (push_X true 0 false blockX, iX)))
			)
		)
		| false, false ->
		(
			mnode()
		)
	)

let compare_subs = List.fold_left2 (fun (x_sub_y, y_sub_x, max_xy, equal) x y -> match x, y with
(* TODO the equal component is not used, remove it *)
		| S, S			-> (x_sub_y, y_sub_x, max_xy, equal)
		| S, P			-> (false  , y_sub_x, max_xy, false)
		| P, S			-> (x_sub_y, false  , max_xy, false)
		| P, P			-> (x_sub_y, y_sub_x, max_xy, equal)
		| S, X(b, i)
		| X(b, i), S	-> (false  , false  , Tools.opmin i max_xy, false)
		| P, X(b, i)	-> (x_sub_y, false  , Tools.opmin i max_xy, false)
		| X(b, i), P	-> (false  , y_sub_x, Tools.opmin i max_xy, false)
		| X(b, i), X(b', i') ->
			if (b = b') && (i = i')
			then		   (x_sub_y, y_sub_x, max_xy, equal)
			else		   (false  , false  , Tools.opmin (min i i') max_xy, false)
	) (true, true, None, true) 



let solve_cons_2 (e0, i0) (e1, i1) =
	let subC, (subX, subY) = GUtils.consensus (function
		| P, P -> (P, None)
		| X(b, i), X(b', i') -> assert(b = b' && i = i'); (X(b, i), None)
		| X _, _
		| _, X _ -> assert false
		| (x, y) -> (S, Some(x, y))) e0.sub e1.sub in
	final_solve_cons
	{
		neg		= e0.neg;
		shift	= e0.shift;
		sub		= S::subC;
	}
	({
		neg = false;
		shift = e0.shift;
		sub = subX;
	}, i0)
	({
		neg = e0.neg <> e1.neg;
		shift = e1.shift;
		sub = subY;
	}, i1)
	

let solve_cons_1 rank (e0, i0) (e1, i1) =
	let sub, (sub0, sub1) = GUtils.consensus (function
		| P, P -> (P, None)
		| X(b, i), X(b', i') when (b = b') && (i = i') && (i <= rank) -> (X(b, i), None)
		| (x, y) -> (S, Some(x, y))) e0.sub e1.sub in
	final_solve_cons {
		neg		= e0.neg;
		shift	= e0.shift;
		sub		= S::sub;
	}
	({
		neg		= false;
		shift	= e0.shift;
		sub		= sub0;
	}, i0)
	({
		neg		= e1.neg <> e0.neg;
		shift	= e1.shift;
		sub		= sub1;
	}, i1)


let solve_cons_0 (e0, i0) (e1, i1) =
	(* X-less merging *)
	let subC, (subX, subY) = GUtils.consensus ( function
		| (P, P) -> (P, None)
		| (x, y) -> (S, Some(x, y))
	) e0.sub e1.sub in
	final_solve_cons
	{
		neg = e0.neg;
		shift = e0.shift;
		sub = S::subC;
	}
	({
		neg = false;
		shift = e0.shift;
		sub = subX;
	}, i0)
	({
		neg = e1.neg <> e0.neg;
		shift = e1.shift;
		sub = subY;
	}, i1)

let solve_cons getid ((e0, i0) as f0) ((e1, i1) as f1) =
	assert(check e0);
	assert(check e1);
	let cmp = CpGops.cmpid getid (i0, i1) in
	if (e0 = e1)
	then if (cmp = None)
		(* the first variable is not significant *)
		then (Utils.MEdge (push_P e0, i0))
		(* the first variable is significant, the GUtils.consensus is obvious *)
		else (Utils.MNode (push_S e0, (cmake_nSS (e0.sub), i0, i1)))
	else	  match block_is_const e0 with
	(* f0 is a constant, thus mergeable *)
	| Some tB -> (Utils.MEdge (push_X false 0 tB e1, i1))
	| None -> match block_is_const e1 with
	(* f1 is a constant, thus mergeable *)
	| Some tB -> (Utils.MEdge (push_X true  0 tB e0, i0))
	| None ->
	(* at this point all trivial merging & GUtils.consensus have been eliminated *)
	if ((e0.neg <> e0.shift) = (e1.neg <> e1.shift))
	then
	(
		(* mergeable *)
		let hasS0, maxX0 = classify e0
		and hasS1, maxX1 = classify e1 in
		match maxX0, maxX1 with
		| Some maxX0, Some maxX1 ->
		(
			let x_sub_y, y_sub_x, max_xy, equal = compare_subs e0.sub e1.sub in
			if equal
			then solve_cons_2 f0 f1
			else
			(
				assert(not(x_sub_y && y_sub_x)); (* they can't be both a subset of each other without being equal *)
				match max_xy with
				| Some max_xy -> (solve_cons_1 max_xy f0 f1)
				(* no X-related conflict has been detected, thus we focus only on P-related problems *)
				| None -> solve_cons_2 f0 f1
			)
		)
		(* X-less merging *)
		| _ -> solve_cons_0 f0 f1
	)
	(* X-less merging *)
	else (solve_cons_0 f0 f1)

let pull_S i block =
	(*print_string"block: "; print_string(CpxDL.block_dummydump block); print_newline();*)
	let f = block.shift <> (mod2 i) in
	let ethen = {
		neg = block.neg <> f;
		shift = block.shift <> f;
		sub = List.map (function
			| ((X(_, j)) as x)  when j < i -> x
			| _ -> P) block.sub;
	} in
	let cnt = MyList.count (function X(_, j) when i = j -> true | _ -> false) block.sub in
	let eelse = if cnt = 0
	then (if i = 0
	then
	{
		neg		= block.neg;
		shift	= not block.shift;
		sub		= List.map (function X(b, j) -> X(b, j-1) | x -> x) block.sub;
	}
	else
	{
		neg		= block.neg;
		shift	= block.shift;
		sub		= List.map (function X(b, j) when j > i -> X(b, j-2) | x -> x) block.sub;
	})
	else block
	in
	reduce ethen, reduce eelse



let node_pull getid (b, i) = match b.sub with
	| [] -> assert false
	| head::tail -> 
		let b' = {
			neg		= b.neg;
			shift	= b.shift;
			sub		= tail;
		} in match head with
		| S ->	Utils.MNode (fun node ->
			let x', y' = node_split node in
			(compose b' x', compose b' y'))
		| P ->	Utils.MEdge ((b', i), (b', i))
		| X(b, j) ->
		(
			let ethen, eelse = pull_S j b' in
			(*print_string"ethen: "; print_string(CpxDL.block_dummydump ethen); print_newline();
			print_string"eelse: "; print_string(CpxDL.block_dummydump eelse); print_newline();*)
			let ethen = (ethen, Utils.Leaf ())
			and eelse = (eelse, i) in
			(*print_string"b: "; print_string(if b then "true" else "false"); print_newline();*)
			Utils.MEdge (Tools.cswap b (ethen, eelse))
		)


let solve_and_1 (ex, ix) (ey, iy) =
	let sub, subXY = GUtils.consensus0(function
		| (P, P) -> P, None
		| (x, y) -> S, Some(x, y)) ex.sub ey.sub in
	Utils.MNode (
		{
			neg = false;
			shift = false;
			sub;
		},
		(
			{
				negX = ex.neg;
				negY = ey.neg;
				shiftX = ex.shift;
				shiftY = ey.shift;
				subXY;
			},
			ix,
			iy
		)
	)

let solve_and_2 (ex, ix) (ey, iy) =
	let sub, subXY = GUtils.consensus0(function
		| (P, P)			-> P, None
		| X(b, 0), X(b', 0) -> assert(b = b'); X(b, 0), None
		| (x, y)			-> S, Some(x, y)
		) ex.sub ey.sub in
	Utils.MNode (
		{
			neg = false;
			shift = false;
			sub;
		},
		(
			{
				negX = ex.neg;
				negY = ey.neg;
				shiftX = ex.shift;
				shiftY = ey.shift;
				subXY;
			},
			ix,
			iy
		)
	)

let solve_and_3 (ex, ix) (ey, iy) =
	let sub, subXY = GUtils.consensus0 (function
		| (P, P)						-> P, None
		| X(b, 0), X(b', 0) when b = b'	-> X(b, 0), None
		| (x, y)						-> S, Some(x, y)
		) ex.sub ey.sub in
	Utils.MNode (
		{
			neg = false;
			shift = true;
			sub;
		},
		(
			{
				negX = ex.neg;
				negY = ey.neg;
				shiftX = ex.shift;
				shiftY = ey.shift;
				subXY;
			},
			ix,
			iy
		)
	)

let solve_and_0 ((blockX, ix) as x) ((blockY, iy) as y) : (block * 'g, block * (block2 * 'g * 'g)) Utils.merge =
	let hasSX, maxXX = classify blockX
	and hasSY, maxXY = classify blockY in
	match maxXX, maxXY with
	| None, None -> solve_and_1 x y
	| Some maxXX, None ->
	(
		solve_and_1 x y
	)
	| None, Some maxXY ->
	(
		solve_and_1 x y
	)
	| Some maxXX, Some maxXY ->
	(
		match blockX.neg <> blockX.shift, blockY.neg <> blockY.shift with
		| false, false ->
		(
			(* look for conflict in level 0 *)
			if List.exists (function X(b, 0), X(b', 0) when b <> b' -> true | _ -> false ) (List.combine blockX.sub blockY.sub)
			then ( Utils.MEdge (get_root false x) )
			else ( solve_and_2 x y )
		)
		| true , true  -> solve_and_3 x y
		| _ -> solve_and_1 x y
	)

let solve_and getid ((ex, ix) as x) ((ey, iy) as y) =
	match node_is_const x with
	| Some b -> Utils.MEdge ( if b then y else x )
	| None -> match node_is_const y with
	| Some b -> Utils.MEdge ( if b then x else y )
	| None ->
	if (CpGops.cmpid getid (ix, iy) = None) && (ex.shift = ey.shift) && (ex.sub = ey.sub)
	then Utils.MEdge (if ex.neg = ey.neg then x (* = y *) else get_root false x)
	else
	( match solve_and_0 x y with
		| Utils.MEdge (e, i) -> Utils.MEdge (reduce e, i)
		| Utils.MNode (e, (exy, ix, iy)) ->
			let e = reduce e in
			let ex, ey = block_split exy in
			let ex = reduce ex
			and ey = reduce ey in
			match block_is_const ex with
			| Some b -> Utils.MEdge (compose e (if b then (ey, iy) else (ex, ix)))
			| None -> match block_is_const ey with
			| Some b -> Utils.MEdge (compose e (if b then (ex, ix) else (ey, iy)))
			| None ->	Utils.MNode (e, (if (ex, ix) < (ey, iy) then (block_merge ex ey, ix, iy) else (block_merge ey ex, iy, ix)))
	)

let solve_xor_0 (ex, ix) (ey, iy) =
	let _, _, max_xy, _ = compare_subs ex.sub ey.sub in
	match max_xy with
	| None ->
	(
		let subC, subXY = List.split(List.map (function(P, P) -> P, None | (X(b, i), X(b', i')) -> assert(b=b' && i=i'); X(b, 0), None | (x, y) -> S, Some(x, y)) (List.combine ex.sub ey.sub)) in
		let subX, subY = List.split(MyList.list_of_oplist subXY) in
		let blockC = {
			neg = ex.neg <> ex.shift <> ey.neg <> ey.shift;
			shift = false;
			sub  = subC;
		}
		and blockX = reduce {
			neg = ex.shift;
			shift = ex.shift;
			sub = subX;
		}
		and blockY = reduce {
			neg = ey.shift;
			shift = ey.shift;
			sub = subY;
		} in
		reduce {
			neg = blockC.neg <> blockX.neg <> blockY.neg;
			shift = blockC.shift <> blockX.neg <> blockY.neg;
			sub = blockC.sub;
		},
		(
			{
				negX = false;
				negY = false;
				shiftX = blockX.shift;
				shiftY = blockY.shift;
				subXY = List.combine blockX.sub blockY.sub;
			},
			ix,
			iy
		)
	)
	| Some max_xy ->
	(
		let subC, subXY = List.split(List.map (function(P, P) -> P, None | (X(b, i), X(b', i')) when b = b' && i = i' && i <= max_xy -> X(b, 0), None | (x, y) -> S, Some(x, y)) (List.combine ex.sub ey.sub)) in
		let subX, subY = List.split(MyList.list_of_oplist subXY) in
		let blockC = {
			neg = ex.neg <> ex.shift <> ey.neg <> ey.shift;
			shift = false;
			sub  = subC;
		}
		and blockX = reduce {
			neg = ex.shift;
			shift = ex.shift;
			sub = subX;
		}
		and blockY = reduce {
			neg = ey.shift;
			shift = ey.shift;
			sub = subY;
		} in
		reduce {
			neg = blockC.neg <> blockX.neg <> blockY.neg;
			shift = blockC.shift <> blockX.neg <> blockY.neg;
			sub = blockC.sub;
		},
		(
			{
				negX = false;
				negY = false;
				shiftX = blockX.shift;
				shiftY = blockY.shift;
				subXY = List.combine blockX.sub blockY.sub;
			},
			ix,
			iy
		)
	)



let solve_xor getid ((ex, ix) as x) ((ey, iy) as y) =
	match node_is_const x with
	| Some b -> Utils.MEdge ( cneg b y )
	| None -> match node_is_const y with
	| Some b -> Utils.MEdge ( cneg b x )
	| None ->
	if (CpGops.cmpid getid (ix, iy) = None) && (ex.shift = ey.shift) && (ex.sub = ey.sub)
	then Utils.MEdge (get_root (ex.neg <> ey.neg) x )
	else
	(
		let e, (xy, ix, iy) = solve_xor_0 x y in
		let ex, ey = block_split xy in
		let ex = reduce ex
		and ey = reduce ey in
		Utils.MNode (e, (if (ex, ix) < (ey, iy)
		then (block_merge ex ey, ix, iy)
		else (block_merge ey ex, iy, ix)))
	)

let solve_xore gid x y = match solve_xor gid x y with
	| Utils.MEdge (edge, gtree) -> Utils.M3Edge (edge, (None, gtree))
	| Utils.MNode (edge, (block, x, y)) -> Utils.M3Node (edge, (block, (None, x), (None, y)))


let node_push_cons gid x y = match solve_cons gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (block, x, y)) -> Utils.MNode (e, (CpxDL.bindump_node block, x, y))

let tacx_push_cons gid x y = match solve_cons gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (block, x, y)) -> Utils.MNode (e, (CpxDL.bindump_tacx (TacxTypes.Cons, block), x, y))

let node_push_and gid (x, y) = match solve_and gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (block, x, y)) -> Utils.MNode (e, (CpxDL.bindump_node block, x, y))

let tacx_push_and gid x y = match solve_and gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (block, x, y)) -> Utils.MNode (e, (CpxDL.bindump_tacx (TacxTypes.And, block), x, y))

let node_push_xor gid (x, y) = match solve_xor gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (block, x, y)) -> Utils.MNode (e, (CpxDL.bindump_node block, x, y))

let tacx_push_xor gid x y = match solve_xor gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (block, x, y)) -> Utils.MNode (e, (CpxDL.bindump_tacx (TacxTypes.Xor, block), x, y))


let tacx_pull_node _ (c, ix, iy) =
	let t, ex, ey = tacx_split (CpxDL.binload_tacx c) in
	(t, (ex, ix), (ey, iy))

let tacx_pull gid e = assert false

let tacx_push gid = TacxTypes.(function
	| Cons -> tacx_push_cons gid
	| And  -> tacx_push_and  gid
	| Xor  -> tacx_push_xor  gid)

let contiguify (block, ident) =
	let min_bigger_than x sub = List.fold_left (fun opmin -> function X(b, i) when i > x -> Tools.opmin i opmin | _ -> opmin) None sub in
	let dec_bigger_than x dec sub = List.map (function X(b, i) when i > x -> X(b, i-dec) | x -> x) sub in
	let min sub = List.fold_left (fun opmin -> function X(b, i) -> Tools.opmin i opmin | _ -> opmin) None sub in
	let dec dec sub =
		if dec = 0
		then sub
		else (List.map (function X(b, i) -> X(b, i-dec) | x -> x) sub)
	in
	match min block.sub with
	| None -> ({neg = block.neg; shift = false; sub = block.sub}, ident)
	| Some min ->
	(
		let rec aux pos sub = match min_bigger_than pos sub with
			| None -> sub
			| Some min ->
			(
				let diff = min - pos in
				let dec = diff - (diff mod 2) in
				aux (if diff mod 2 = 0 then pos else (pos+1)) (if dec = 0 then sub else (dec_bigger_than pos dec sub))
			)
		in
		let block = {neg = block.neg; shift = block.shift<>(mod2 min); sub = aux 0 (dec min block.sub)} in
		((match ident with Utils.Leaf () -> reduce_0 block | Utils.Node _ -> block), ident)
	)

	

let assign = function
	| None -> fun block -> block, None
	| Some set -> fun (block, ident) ->
		assert(List.length set = List.length block.sub);
		let foldmap f i l =
			let rec aux l' i = function
				| [] -> List.rev l', i
				| head::tail ->
					let e, i = f i head in
					aux (e::l') i tail
			in aux [] i l
		in
		let opsubopset, opmin = foldmap (fun opmin -> fun (set, sub) -> match set with
			| None	   -> ( Some sub, (match sub with S -> Some None | _ -> None)), opmin
			| Some set -> match sub with
				| P		  -> (None    , None     ), opmin
				| S		  -> (None    , Some (Some set) ), opmin
				| X(b, i) -> (None    , None     ), if b = set then Tools.opmin i opmin else opmin
			) None (List.combine set block.sub) in
		let opsub, opset = List.split opsubopset in
		let sub = MyList.list_of_oplist opsub
		and set = MyList.list_of_oplist opset in
		match opmin with
		| None -> ((contiguify ({neg = block.neg; shift = block.shift; sub = sub}, ident)), (if List.for_all ((=)None) set then None else (Some set)))
		| Some min -> ((contiguify ({neg = block.neg <> block.shift <> (mod2 min); shift = (mod2 min); sub = List.map (function S | P -> P | X(_, i) when i >= min -> P | (X _) as x -> x) sub}, Utils.Leaf())), None)


let assign_dummydump = StrDump.(option(list(option int)))

let solve_ande_P2 (ex, ix) (ey, iy) =
	let sub, subXY = GUtils.consensus0 (function
		| (P, P) -> P, None
		| (x, y) -> S, Some(x, y)) ex.sub ey.sub in
	Utils.M3Node (
		{
			neg = false;
			shift = false;
			sub;
		},
		(
			{
				negX = ex.neg;
				negY = ey.neg;
				shiftX = ex.shift;
				shiftY = ey.shift;
				subXY;
			},
			(None, ix),
			(None, iy)
		)
	)

let solve_ande_P2X2_11 (ex, ix) (ey, iy) =
	(*print_string "solve_ande_P2PX2_11"; print_newline();*)
	let sub, subXY = GUtils.consensus0 (function
		| P, P -> P, None
		| X(b, 0), X(b', 0) when b = b' -> X(b, 0), None
		| (x, y) -> S, Some(x, y)) ex.sub ey.sub in
	Utils.M3Node (
		{
			neg = false;
			shift = true;
			sub;
		},
		(
			{
				negX = ex.neg;
				negY = ey.neg;
				shiftX = ex.shift;
				shiftY = ey.shift;
				subXY;
			},
			(None, ix),
			(None, iy)
		)
	)

exception Return_False

let solve_ande_P2X2 bX bY (ex, ix) (ey, iy) =
	let bX, ex = if (bX = false) && (block_is_singleton ex)
		then (true,(
			assert(ex.shift = true);
			{neg = not ex.neg; shift = true; sub = List.map (function X(b, i) -> assert(i = 0); X(not b, 0) | x -> x) ex.sub};
		))
		else (bX, ex)
	and bY, ey = if (bY = false) && (block_is_singleton ey)
		then (true,(
			assert(ey.shift = true);
			{neg = not ey.neg; shift = true; sub = List.map (function X(b, i) -> assert(i = 0); X(not b, 0) | x -> x) ey.sub};
		))
		else (bY, ey)
	in
	if (not bX) && (not bY)
	then ( solve_ande_P2X2_11 (ex, ix) (ey, iy) )
	else try
	(
		let sub, setsubXsetsubY = List.split(List.map(function
			| P, P -> P, (None, None)
			| X(b, 0), X(b', 0) when bX && bY -> if b = b' then (X(b, 0), (None, None)) else (raise Return_False) 
			| X(b, 0), y		when bX -> X(b, 0), (None, Some(Some(not b), y))
			| x, X(b, 0)		when bY -> X(b, 0), (Some(Some(not b), x), None)
			| x, y				-> S, (Some(None, x), Some(None, y))
		) (List.combine ex.sub ey.sub)) in
		let setsubX, setsubY = List.split setsubXsetsubY in
		let setX, subX = List.split(MyList.list_of_oplist setsubX)
		and setY, subY = List.split(MyList.list_of_oplist setsubY) in
		let setX = if List.for_all (function None -> true | _ -> false) setX then None else Some setX
		and setY = if List.for_all (function None -> true | _ -> false) setY then None else Some setY in
		let subX = if bX then (List.map (function X(b, i) -> assert(i>0); X(b, i-1) | x -> x) subX) else subX
		and subY = if bY then (List.map (function X(b, i) -> assert(i>0); X(b, i-1) | x -> x) subY) else subY in
		let ex = {neg = ex.neg; shift = ex.shift<>bX; sub = subX}
		and ey = {neg = ey.neg; shift = ey.shift<>bY; sub = subY} in
		let (ex, ix), setX = assign setX (ex, ix)
		and (ey, iy), setY = assign setY (ey, iy) in
		Utils.M3Node (
			{
				neg = false;
				shift = false;
				sub;
			},
			(
				{
					negX = ex.neg;
					negY = ey.neg;
					shiftX = ex.shift;
					shiftY = ey.shift;
					subXY = List.combine ex.sub ey.sub;
				},
				(setX, ix),
				(setY, iy)
			)
		)
	)
	with Return_False -> ( let edge, gtree = get_root false (ex, ix) in Utils.M3Edge (edge, (None, gtree)) )

let solve_ande_0 ((blockX, ix) as x) ((blockY, iy) as y) =
	(*print_string "solve_ande_0"; print_newline();
	print_string "\tblockX: "; print_string(CpxDL.edge_dummydump (blockX, ix)); print_newline();
	print_string "\tblockY: "; print_string(CpxDL.edge_dummydump (blockY, iy)); print_newline();*)
	let _, maxXX = classify blockX
	and _, maxXY = classify blockY in
	if (maxXX = None) && (maxXY = None)
	then ( solve_ande_P2 x y )
	else
	(
		let tx = blockX.neg <> blockX.shift
		and ty = blockY.neg <> blockY.shift in
		solve_ande_P2X2 (not tx) (not ty) x y
	)

let compose blockC (blockc, e) =
	(*print_string "compose: "; print_newline();
	print_string "\tblockC: "; print_string(CpxDL.block_dummydump blockC); print_newline();
	print_string "\tblockc: "; print_string(CpxDL.edge_dummydump (blockc, e)); print_newline();*)
	let edge = compose blockC (blockc, e) in
	(*print_string "\tblockCc: "; print_string(CpxDL.edge_dummydump edge); print_newline();*)
	edge

let solve_ande getid ((ex, ix) as x) ((ey, iy) as y) =
	match node_is_const x with
	| Some b -> Utils.M3Edge ( let edge, gtree = if b then y else x in (edge, (None, gtree)))
	| None -> match node_is_const y with
	| Some b -> Utils.M3Edge ( let edge, gtree = if b then x else y in (edge, (None, gtree)))
	| None ->
	if (CpGops.cmpid getid (ix, iy) = None) && (ex.shift = ey.shift) && (ex.sub = ey.sub)
	then Utils.M3Edge (let edge, gtree = if ex.neg = ey.neg then x (* = y *) else get_root false x in (edge, (None, gtree)))
	else
	( match solve_ande_0 x y with
		| Utils.M3Edge (e, (ope, i)) -> Utils.M3Edge (reduce' e, (ope, i))
		| Utils.M3Cons (e, (e0, e1)) -> Utils.M3Cons (e, (e0, e1))
		| Utils.M3Node (e, (exy, (opex, ix), (opey, iy))) ->
			let sub, subXY = List.split(List.map(function(P, P) -> P, None | (x, y) -> S, Some(x, y))exy.subXY) in
			let subXY = MyList.list_of_oplist subXY in
			let e = compose_block' e {neg = false; shift = false; sub}
			and exy = {negX = exy.negX; negY = exy.negY; shiftX = exy.shiftX; shiftY = exy.shiftY; subXY} in
			assert(not(List.exists (function(P, P) -> true | _ -> false) exy.subXY));
			if (opex = None) && (opey = None)
			then
			(
				let ex, ey = block_split exy in
				(*print_string "[POST] solve_ande_0"; print_newline();
				print_string "e: "; print_string(CpxDL.block_dummydump e); print_newline();
				print_string "ex: "; print_string(CpxDL.edge_dummydump (ex, ix)); print_newline();
				print_string "ey: "; print_string(CpxDL.edge_dummydump (ey, iy)); print_newline();*)
				let e = reduce' e in
				let ex = reduce ex
				and ey = reduce ey in
				(*print_string "[POST] solve_ande_0"; print_newline();
				print_string "e: "; print_string(CpxDL.block_dummydump e); print_newline();
				print_string "blockX: "; print_string(CpxDL.edge_dummydump (ex, ix)); print_newline();
				print_string "blockY: "; print_string(CpxDL.edge_dummydump (ey, iy)); print_newline();*)
				match block_is_const ex with
				| Some b -> Utils.M3Edge (let edge, gtree = compose e (if b then (ey, iy) else (ex, ix)) in (edge, (None, gtree)))
				| None -> match block_is_const ey with
				| Some b -> Utils.M3Edge (let edge, gtree = compose e (if b then (ex, ix) else (ey, iy)) in (edge, (None, gtree)))
				| None ->
				(
					let return () = Utils.M3Node (e, (if (ex, ix) < (ey, iy) then (block_merge ex ey, (None, ix), (None, iy)) else (block_merge ey ex, (None, iy), (None, ix)))) in
					assert(List.length ex.sub = List.length ey.sub);
					assert(List.length ex.sub >= 1);
					if (ex.neg <> ex.shift) && (ey.neg <> ex.shift)
					then match ex.sub, ey.sub with
						| X(b, 0)::subX, X(b', 0)::subY when b <> b' ->
						(
							let x = (reduce {neg = ex.neg; shift = ex.shift; sub = subX}, (None, ix))
							and y = (reduce {neg = ey.neg; shift = ey.shift; sub = subY}, (None, iy)) in
							Utils.M3Cons (e, (Tools.cswap b (y, x)))
						)
						| _ -> return()
					else return()
				)
			)
			else
			(
				let return () = Utils.M3Node (e, (exy, (opex, ix), (opey, iy))) in
				if (List.length exy.subXY >= 1) && (exy.negX <> exy.shiftX) && (exy.negY <> exy.shiftY)
				then match exy.subXY with
					| [] -> assert false
					| (X(b, 0), X(b', 0))::subXY when b <> b' ->
					(
						let ex, ey = CpxUtils.block_split {negX = exy.negX; negY = exy.negY; shiftX = exy.shiftX; shiftY = exy.shiftY; subXY } in
						let x = (reduce {neg = ex.neg; shift = ex.shift; sub = ex.sub}, (opex, ix))
						and y = (reduce {neg = ey.neg; shift = ey.shift; sub = ey.sub}, (opey, iy)) in
						Utils.M3Cons (e, (Tools.cswap b (y, x)))
					)
					| _ -> return()
				else return()
			)
			(*(
				(*print_string "[PROPA] solve_ande_0"; print_newline();
				let ex, ey = block_split exy in
				print_string "\te: "; print_string(CpxDL.block_dummydump e); print_newline();
				print_string "\tex: "; print_string(CpxDL.edge_dummydump (ex, ix)); print_newline();
				print_string "\tey: "; print_string(CpxDL.edge_dummydump (ey, iy)); print_newline();
				print_string "\tsetX:"; print_string (assign_dummydump opex); print_newline();
				print_string "\tsetY:"; print_string (assign_dummydump opey); print_newline();*)
				Utils.M3Node (e, (exy, (opex, ix), (opey, iy)))
			)*)
	)


let node_push_ande gid (x, y) = match solve_ande gid x y with
	| Utils.M3Edge e -> Utils.M3Edge e
	| Utils.M3Cons (e, (e0, e1)) -> Utils.M3Cons (e, (e0, e1))
	| Utils.M3Node (e, (block, x, y)) -> Utils.M3Node (e, (CpxDL.bindump_node block, x, y))

let tacx_propa_cons gid x y = match solve_cons gid x y with
	| Utils.MEdge (edge, gtree) -> Utils.MEdge (edge, (None, gtree))
	| Utils.MNode (e, (block, gtreeX, gtreeY)) ->
	(
		let edgeX, edgeY = block_split block in
		Utils.MNode (e, (TacxTypes.Cons, (edgeX, (None, gtreeX)), (edgeY, (None, gtreeY))))
	)

let tacx_propa_and gid x y = match solve_ande gid x y with
	| Utils.M3Edge e -> Utils.MEdge e
	| Utils.M3Cons (e, (eogX, eogY)) -> Utils.MNode (e, (TacxTypes.Cons, eogX, eogY))
	| Utils.M3Node (e, (block, ogX, ogY)) ->
	(
		let edgeX, edgeY = block_split block in
		Utils.MNode (e, (TacxTypes.And, (edgeX, ogX), (edgeY, ogY)))
	)

let tacx_propa_xor gid x y = match solve_xore gid x y with
	| Utils.M3Edge e -> Utils.MEdge e
	| Utils.M3Cons (e, (eogX, eogY)) -> Utils.MNode (e, (TacxTypes.Cons, eogX, eogY))
	| Utils.M3Node (e, (block, ogX, ogY)) ->
	(
		let edgeX, edgeY = block_split block in
		Utils.MNode (e, (TacxTypes.Xor, (edgeX, ogX), (edgeY, ogY)))
	)

let tacx_propa gid = TacxTypes.(function
	| Cons -> tacx_propa_cons gid
	| And  -> tacx_propa_and  gid
	| Xor  -> tacx_propa_xor  gid)
