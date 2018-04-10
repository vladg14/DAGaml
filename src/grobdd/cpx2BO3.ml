(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open Extra
open O3Extra
open Cpx2BTypes
open Cpx2BUtils

let log2 = Tools.math_log_up 2

let is_nil (*next_is_leaf*) block = match block.block with
	| C0 | Id _ -> true
	| SPX _ -> false

let is_nil2 (blockX, blockY) = (is_nil blockX, is_nil blockY)

let bindump_elem_spx size elem stream = match elem with
	| S -> true::stream
	| P -> false::true::stream
	| X(b, i) -> false::false::b::(BinDump.sized_int size i stream)

let binload_elem_spx size = function
	| true ::stream -> S, stream
	| false::true::stream -> P, stream
	| false::false::b::stream ->
		let i, stream = BinLoad.sized_int size stream in
		X(b, i), stream
	| _ -> assert false

let bindump_elem_sx size elem stream = match elem with
	| S -> true::stream
	| P -> assert false
	| X(b, i) -> false::b::(BinDump.sized_int size i stream)

let binload_elem_sx size = function
	| true::stream -> S, stream
	| false::b::stream ->
		let i, stream = BinLoad.sized_int size stream in
		X(b, i), stream
	| _ -> assert false

let bindump_elem_sp elem stream = match elem with
	| S -> true ::stream
	| P -> false::stream
	| X _ -> assert false

let binload_elem_sp = function
	| true ::stream -> S, stream
	| false::stream -> P, stream
	| _ -> assert false

let bindump_elem_px size elem stream = match elem with
	| S -> assert false
	| P -> true ::stream
	| X(b, i) -> false::b::(BinDump.sized_int size i stream)

let binload_elem_px size = function
	| true ::stream -> P, stream
	| false::b::stream ->
		let i, stream = BinLoad.sized_int size stream in
		X(b, i), stream
	| _ -> assert false

let bindump_elem_x size elem stream = match elem with
	| S | P -> assert false
	| X(b, i) -> b::(BinDump.sized_int size i stream)

let binload_elem_x size = function
	| b::stream ->
		let i, stream = BinLoad.sized_int size stream in
		X(b, i), stream
	| _ -> assert false

let bindump_block block next_is_leaf stream =
	assert(check_block block next_is_leaf);
	block.neg::(BinDump.int block.arity (match block.arity with
		| 0 ->
		(
			(if next_is_leaf
			then (assert(block.block = C0))
			else (assert(block.block = SPX(false, {hasS = false; hasP = false; maxX = None}, []))));
			stream
		)
		| n ->
		(
			let logn = log2 (n+1) in
			match block.block with
			| C0 -> false::stream
			| Id x -> true::false::(BinDump.sized_int logn x stream)
			| SPX(shift, tag, liste) ->
			let store_hasS stream = if next_is_leaf
				then (assert(tag.hasS = false); stream)
				else (tag.hasS::stream)
			in
			let store_maxX stream =
				let logm = log2 (n+2) in
				match tag.maxX with
				| None   -> BinDump.sized_int logm    0  stream
				| Some x -> BinDump.sized_int logm (x+1) stream
			in
			let dumpliste dump : bool list =
				BinDump.sized_list dump liste stream
			in
			true::true::(store_hasS(tag.hasP::(store_maxX(match tag.maxX with
				| None ->
				(
					assert(shift = false);
					if tag.hasS && tag.hasP
						then (dumpliste bindump_elem_sp )
						else stream
				)
				| Some maxX ->
				let dumpliste' dump = dumpliste (dump (log2 (maxX+1))) in
				shift::(if tag.hasS
					then if tag.hasP
						then (dumpliste' bindump_elem_spx)
						else (dumpliste' bindump_elem_sx )
					else if tag.hasP
						then (dumpliste' bindump_elem_px )
						else (dumpliste' bindump_elem_x  )
				)
		))))
	)))

let bindump_edge block stream =
	let nil = is_nil block in
	nil::(bindump_block block nil stream)


let binload_block next_is_leaf stream : block * (bool list)=
	let neg, stream = BinLoad.bool stream in
	let arity, stream = BinLoad.int stream in
	let block, stream = match arity with
	| 0 ->
	((if next_is_leaf
		then C0
		else SPX(false, {hasS = false; hasP = false; maxX = None}, [])
	), stream)
	| n ->
	(
		let logn = log2 (n+1) in
		match stream with
		| false::stream -> (C0, stream)
		| true ::false::stream ->
		(
			let x, stream = BinLoad.sized_int logn stream in
			(Id x, stream)
		)
		| true ::true ::stream ->
		(
			let load_hasS stream = if next_is_leaf
				then (false, stream)
				else (BinLoad.bool stream)
			in
			let load_maxX stream =
				let logm = log2 (n+2) in
				let maxx, stream = BinLoad.sized_int logm stream in
				((if maxx = 0 then None else (Some (maxx-1))), stream)
			in
			let hasS, stream = load_hasS stream in
			let hasP, stream = BinLoad.bool stream in
			let maxX, stream = load_maxX stream in
			let tag = {hasS; hasP; maxX} in
			let loadliste load stream =
				BinLoad.sized_list load arity stream
			in
			let shift, (liste, stream) = match maxX with
				| None ->
				(false, (if hasS
					then if hasP
						then (loadliste binload_elem_sp stream)
						else (MyList.ntimes S arity, stream)
					else if hasP
						then (MyList.ntimes P arity, stream)
						else (assert(arity = 0); ([], stream))
				))
				| Some maxX ->
				(
					let shift, stream = BinLoad.bool stream in
					let loadliste' (load : int -> bool list -> _ * bool list) : (_ list) * (bool list) =
						loadliste (load (log2 (maxX+1))) stream
					in
					(shift, loadliste' (if hasS
					then if hasP
						then binload_elem_spx
						else binload_elem_sx
					else if hasP
						then binload_elem_px
						else binload_elem_x
					))
				)
			in
			(SPX(shift, tag, liste), stream)
		)
		| _ -> assert false
	) in
	{neg; arity; block}, stream

let binload_edge       stream =
	let nil, stream = BinLoad.bool stream in
	binload_block nil stream

let o3s_edge = (bindump_edge, binload_edge)

let o3s_node = O3.trio (BinO3.unit, o3s_edge, o3s_edge)

let conv_next = Utils.(function
	| Leaf leaf -> Leaf leaf
	| Node node -> Node 0
)

let conv_next2 = Utils.(function
	| Node node0, Node node1 ->
	(
		if node0 = node1
			then (Node 0, Node 0)
		else if node0 < node1
			then (Node 0, Node 1)
			else (Node 1, Node 0)
	)
	| next0, next1 -> (conv_next next0, conv_next next1)
)

let conv_node (node, (edge0, next0), (edge1, next1)) =
	let next0, next1 = conv_next2 (next0, next1) in
	(node, (edge0, next0), (edge1, next1))

let o3s_next' = Utils.o3s_gnode BinO3.unit BinO3.int
let o3s_node' = O3Utils.from_a_bc_de_to_abd_c_e +>> O3.(trio (o3s_node, o3s_next', o3s_next'))
let o3b_node' = BinO3.closure o3s_node'
let o3str_node' = o3b_node' %>> StrO3.bitv_hexa

let o3s_tacx  = O3.trio (TacxTypes.o3s_tag, o3s_edge, o3s_edge)
let o3b_tacx  = BinO3.closure o3s_tacx
let o3str_tacx  = o3b_tacx  %>> StrO3.bitv_hexa

let o3s_tacx' = O3Utils.from_a_bc_de_to_abd_c_e +>> O3.(trio (o3s_tacx, o3s_next', o3s_next'))
	

let bindump_block2 (blockX, blockY) (next_is_leafX, next_is_leafY) stream =
	bindump_block blockX next_is_leafX (bindump_block blockY next_is_leafY stream)

let binload_block2 (next_is_leafX, next_is_leafY) stream =
	let blockX, stream = binload_block next_is_leafX stream in
	let blockY, stream = binload_block next_is_leafY stream in
	((blockX, blockY), stream)

let pretty_block_spx neg arity (shift, tag, liste) =
	let pretty_x iB tB = match iB, tB with
		| false, false -> "1"
		| true , false -> "0"
		| false, true  -> "I"
		| true , true  -> "O"
	in
	let floor () = StrUtil.catmap "" (function S -> "S" | P -> "P" | X(iB, 0) -> pretty_x iB shift | X _ -> ".") liste in
	match tag.maxX with
	| None ->
	(if tag.hasS && tag.hasP
		then ((GUtils.mp_of_bool neg)^(floor ()))
		else ("("^(GUtils.mp_of_bool neg)^" "^(string_of_int arity)^" "^(if tag.hasS then "S" else "P")^")")
	)
	| Some 0 -> ((GUtils.mp_of_bool neg)^(floor ()))
	| Some maxX ->
	(
		let uppers : string list = MyList.init maxX (fun i ->
			let i = i+1 in
			let tB = shift <> (mod2 i) in
			let funmap = function
				| X(iB, j) ->
				(if i < j
					then "."
					else if i = j
					then (pretty_x iB tB)
					else " "
				)
				| _ -> " "
			in
			" "^(StrUtil.catmap "" funmap liste)
			)
		in ((String.concat "\n" ((List.rev ((floor())::uppers)):string list)):string)
	)

let pretty_block block = match block.block with
	| C0 -> ("("^(GUtils.mp_of_bool block.neg)^" "^(string_of_int block.arity)^" C0 )")
	| Id x -> ("("^(GUtils.mp_of_bool block.neg)^" "^(string_of_int block.arity)^" Id "^(string_of_int x)^")")
	| SPX block_spx -> pretty_block_spx block.neg block.arity block_spx
