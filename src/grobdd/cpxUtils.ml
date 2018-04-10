(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open CpxTypes

let block_split block =
	let subX, subY = List.split block.subXY in
	{
		neg = block.negX;
		shift = block.shiftX;
		sub = subX
	},
	{
		neg = block.negY;
		shift = block.shiftY;
		sub = subY;
	}

let block_merge blockX blockY =
	{
		negX	= blockX.neg;
		shiftX	= blockX.shift;
		negY	= blockY.neg;
		shiftY	= blockY.shift;
		subXY	= List.combine blockX.sub blockY.sub;
	}

let node_split (block, iX, iY) =
	let blockX, blockY = block_split block in
	(blockX, iX), (blockY, iY)

let node_merge (blockX, iX) (blockY, iY) =
	(block_merge blockX blockY, iX, iY)

(* return true if x is odd, false otherwise *)
let mod2 x = (x mod 2) = 1

let block_size block = List.length block.sub
let node_size (block, _) = block_size block

(* check that a block is properly formed *)
(* TODO
	- check for trailing 0s
	- check that shift is set to 0 if their is no X elem
	- check for a single trailing 1
	- check for sub-normal case +/- I (if false then true else false)
*)
let check_0 block = (* check for contiguity *)
	let n = block_size block in
	let cnt = Array.make n 0 in
	let clk x = cnt.(x) <- cnt.(x) + 1 in
	List.iter (function
		| S | P -> ()
		| X (_, x) -> clk x) block.sub;
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
	aux1 (Array.to_list cnt)

let block_is_const block =
	if List.for_all (function P -> true | _ -> false) block.sub
	then Some block.neg
	else None

let node_is_const (block, i) = block_is_const block

let push_S block = {
	neg = block.neg;
	shift = block.shift;
	sub = S::block.sub;
}

let push_P block = {
	neg		= block.neg;
	shift	= block.shift;
	sub		= P::block.sub;
}

let classify block = List.fold_left (fun (hasS, maxX) -> function
		| S			-> (true, maxX					)
		| P			-> (hasS, maxX					)
		| X(b, i)	-> (hasS, Tools.opmax i maxX	)
	) (false, None) block.sub

let in_block block x =
	let hasS, maxX = classify block in
	match maxX with
	| None -> if hasS then None else (Some block.neg)
	| Some maxX ->
	(
		match List.fold_left (fun opmin -> function (X(b, i), b') when b = b' -> Tools.opmin i opmin | _ -> opmin) None (List.combine block.sub x) with
		| None -> if hasS then None else (Some block.neg)
		| Some i -> Some(block.neg <> block.shift <> (mod2 i))
	)

let check block =
	(check_0 block)&&(
	let hasS, maxX = classify block in
	match maxX with
	| None -> block.shift = false
	| Some maxX ->
	(
		if hasS
		then true
		else (
			(block.shift <> (mod2 maxX)) && (
			let n = MyList.count (function X(_, i) when i = maxX -> true | _ -> false) block.sub in
			assert(n>=1);
			if (maxX = 0) && (n = 1)
			then (List.for_all (function X(true, _) -> false | _ -> true) block.sub)
			else (n > 1)
			)
		)
	))

let reduce_0 block =
	match (List.fold_left (fun opmax -> function X(_, i) -> Tools.opmax i opmax | _ -> opmax) None block.sub) with
	| None ->
	{
		neg   = block.neg;
		shift = false;
		sub   = block.sub;
	}
	| Some maxX ->
	(
		let block, maxX = if block.shift <> (mod2 maxX) then block, maxX else
		{
			neg		= block.neg;
			shift	= block.shift;
			sub		= List.map (function X(_, i) when i = maxX -> P | x -> x) block.sub;
		}, maxX - 1 in
		if maxX = - 1
		then
		{
			neg		= block.neg;
			shift	= false;
			sub		= block.sub;
		}
		else
		(
			let n = MyList.count (function X(_, i) -> i = maxX | _ -> false) block.sub in
			assert(n>=1);
			if n = 1
			then ( if maxX = 0
			then
			(
				(* single significant variable *)
				let iB = match (List.fold_left   ( function
						| Some x -> (fun _ -> Some x)
						| None   -> function
						| S | P -> None
						| X(b, 0) -> Some b
						| _       -> assert false ) None block.sub) with
					| None -> assert false
					| Some iB -> iB
				in
				{
					neg	  = block.neg <> iB;
					shift = true;
					sub	  = List.map (function X _ -> X(false, 0) | x -> x) block.sub;
				}
			)
			else
			{
				neg   = not block.neg;
				shift = not block.shift;
				sub   = List.map (function X(b, i) when i = maxX -> X(not b, i-1) | x -> x) block.sub;
			}
				)
			else block
		)
	)

let reduce block =
	let minX = List.fold_left (fun x -> function S | P -> x | X(_, i) -> Tools.opmin i x) None block.sub in
	match minX with
	| None ->
	{
		neg		= block.neg;
		shift	= false;
		sub		= block.sub;
	}
	| Some minX ->
	(
		assert(minX>=0);
		let block = {
			neg		= block.neg;
			shift	= block.shift <> (mod2 minX);
			sub		= if minX = 0
			then block.sub
			else List.map (function
				| S -> S
				| P -> P
				| X(b, i) -> X(b, i-minX)) block.sub
		} in
		if List.exists (function S -> true | _ -> false) block.sub
		then block
		else (reduce_0 block)
	)

let reduce' block =
	let minX = List.fold_left (fun x -> function S | P -> x | X(_, i) -> Tools.opmin i x) None block.sub in
	match minX with
	| None ->
	{
		neg		= block.neg;
		shift	= false;
		sub		= block.sub;
	}
	| Some minX ->
	(
		assert(minX>=0);
		{
			neg		= block.neg;
			shift	= block.shift <> (mod2 minX);
			sub		= if minX = 0
			then block.sub
			else List.map (function
				| S -> S
				| P -> P
				| X(b, i) -> X(b, i-minX)) block.sub
		}
	)

let node_reduce (block, gtree) = match gtree with
	| Utils.Leaf () -> (reduce  block, gtree)
	| Utils.Node _  -> (reduce' block, gtree)

let push_X iB rN tB (* if, rank, then *) block =
	assert(rN >= 0);
	let hasS, maxX = classify block in
	match maxX with
	| None ->
	(
		assert(rN = 0);
		assert(block.shift = false);
		if hasS
		then
		{
			neg		= block.neg;
			shift	= tB <> block.neg;
			sub		= (X(iB, 0))::block.sub;
		}
		else if (tB <> block.neg) = false
		then
		(* trailing 0 *)
		{
			neg		= block.neg;
			shift	= false;
			sub		= P::block.sub;
		}
		else
		(* trailing 1 *)
		{
			neg = block.neg <> iB;
			shift = true;
			sub = (X(false, 0))::block.sub;
		}
	)
	| Some maxX ->
	(
		assert(maxX >= 0);
		assert(rN <= maxX + 1);
		if(not hasS && rN = maxX + 1)
		then
		(* trailing 1 *)
		{
			neg		= not block.neg;
			shift	= not block.shift;
			sub		= (X(not iB, maxX))::block.sub;
		}
		else if (tB <> block.neg <> block.shift <> (mod2 rN)) = false
		then
		(* correct alignment *)
		{
			neg		= block.neg;
			shift	= block.shift;
			sub		= (X(iB, rN))::block.sub;
		}
		else
		(
			(* incorrect alignment *)
			if rN = 0
			then
			(
				let sub' = (X(iB, rN))::(List.map (function
					| S -> S
					| P -> P
					| X(b, i) -> X(b, i+1))) block.sub in
(* TODO : fix this case {neg = true; shift = false; sub = [X (false, 0); X (false, 1)]} *)
				reduce {
					neg		= block.neg;
					shift	= not block.shift;
					sub		= sub';
				}
			)
			else
			(
				{
					neg		= block.neg;
					shift	= block.shift;
					sub		= (X(iB, rN-1))::block.sub;
				}
			)
		)
	)

let push_Xs liste = List.fold_right (function (iB, rB, tB) -> push_X iB rB tB) liste


(* propagate the specialisation *)
let push_XsRT liste rB tB = List.fold_right (function iB -> push_X iB rB tB) liste

let node_push_XsRT liste rB tB (block, gtree) = (push_XsRT liste rB tB block, gtree)

let count_nS liste = MyList.count (function S -> true | _ -> false) liste


let make_nSS n = {
	negX   = false;
	negY   = false;
	shiftX = false;
	shiftY = false;
	subXY  = MyList.ntimes (S, S) n;
}

let  make_nS n = {
	neg   = false;
	shift = false;
	sub   = MyList.ntimes S n;
}

let cmake_nSS block = make_nSS (count_nS block)
let cmake_nS  block = make_nS  (count_nS block)

let tacx_split (ttag, block) =
	let blockX, blockY = block_split block in
	(ttag, blockX, blockY)

let compose_block' blockC blockc =
	let hasS, maxX = classify blockC in
	match maxX with
	| None ->
	(
		let sub = (let rec aux carry = function
			| ([], []) -> List.rev carry
			| ([], _) -> ignore(blockC); ignore(blockc); assert false
			| (S::x', y::y') -> aux (y::carry) (x', y')
			| (S::_, []) -> assert false
			| ((X _)::x' , _) -> assert false
			| (P::x', y') -> aux (P::carry) (x', y') in aux [] (blockC.sub, blockc.sub)) in
		{
			neg		= blockC.neg <> blockc.neg;
			shift	= blockc.shift;
			sub		= sub;
		}
	)
	| Some maxX ->
	(
		let blockC_neg = blockC.neg <> blockc.neg
		and blockC_shift = blockC.shift <> blockc.neg in
		let blockc_dec = maxX + (if ((mod2 maxX) <> blockC_shift) =  blockc.shift then 0 else 1) in
		let sub = (let rec aux carry = function
			| ([], []) -> List.rev carry
			| ([], _ ) -> assert false
			| (S::x', y::y') -> aux ((match y with X(b, i) -> X(b, i+blockc_dec) | _ -> y)::carry) (x', y')
			| (S::_, []) -> assert false
			| (x::x', y') -> aux (x::carry) (x', y') in aux [] (blockC.sub, blockc.sub)) in
		{
			neg		= blockC_neg;
			shift	= blockC_shift;
			sub		= sub;
		}
	)

let compose_block blockC blockc = compose_block' blockC blockc |> reduce

(*let compose bC (bc, ic) = (compose_block bC bc, ic)*)

let compose bC (bc, ic) = match ic with
	| Utils.Leaf () -> (compose_block  bC bc, ic)
	| Utils.Node _  -> (compose_block' bC bc, ic)

let get_root b (block, _) = ({neg = b; shift = false; sub = List.map (fun _ -> P) block.sub}, Utils.Leaf())

let neg (block, i) = ({neg = not block.neg; shift = block.shift; sub= block.sub}, i)
let cneg b (block, i) = ({neg = block.neg <> b; shift = block.shift; sub = block.sub}, i)
