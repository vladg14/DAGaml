(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open CpxTypes
open CpxUtils

let bindump_elem elem stream = match elem with
	| Some x ->
	( match x with
		| S -> true::stream
		| P -> false::true::stream
		| X(b, i) -> false::false::true::b::(BinDump.int i stream)
	)
	| None -> false::false::false::stream

let bindump_elem2 elem2 stream = match elem2 with
	| Some x ->
	( match x with
		| S, S					-> true ::stream
		| S, P					-> false::true::false ::stream
		| P, S					-> false::true::true  ::stream
(*		| P, P					-> false::false::false::false::false::false::stream *)
		| P, P					-> assert false
		| S, X(b, i)			-> false::false::true ::false::b::(BinDump.int i stream)
		| X(b, i), S			-> false::false::true ::true ::b::(BinDump.int i stream)
		| P, X(b, i)			-> false::false::false::true ::false::b::(BinDump.int i stream)
		| X(b, i), P			-> false::false::false::true ::true ::b::(BinDump.int i stream)
		| X(b, i), X(b', i')	-> false::false::false::false::true ::b::b'::(BinDump.int i (BinDump.int i' stream))
	)
	| None						-> false::false::false::false::false::true ::stream

let binload_elem = function
	| true::stream -> Some S, stream
	| false::true::stream -> Some P, stream
	| false::false::true::b::stream -> let i, stream = BinLoad.int stream in Some(X(b, i)), stream
	| false::false::false::stream -> None, stream
	| _ -> assert false

let binload_elem2 = function
	| true::stream -> Some(S, S), stream
	| false::true::false::stream -> Some(S, P), stream
	| false::true::true ::stream -> Some(P, S), stream
	| false::false::true::false::b::stream ->
		let i, stream = BinLoad.int stream in
		Some(S, X(b, i)), stream
	| false::false::true::true ::b::stream ->
		let i, stream = BinLoad.int stream in
		Some(X(b, i), S), stream
	| false::false::false::true::false::b::stream ->
		let i, stream = BinLoad.int stream in
		Some(P, X(b, i)), stream
	| false::false::false::true::true ::b::stream ->
		let i, stream = BinLoad.int stream in
		Some(X(b, i), P), stream
	| false::false::false::false::true::b::b'::stream ->
		let i, stream = BinLoad.int stream in
		let i', stream = BinLoad.int stream in
		Some(X(b, i), X(b', i')), stream
	| false::false::false::false::false::true ::stream -> None, stream
	| false::false::false::false::false::false::stream -> Some(P, P), stream
	| _ -> assert false


let bindump_block block stream =
	block.neg::block.shift::(BinDump.none_list bindump_elem block.sub stream)

let binload_block = function
	| neg::shift::stream ->
		let sub, stream = BinLoad.none_list binload_elem stream in
		{ neg; shift; sub }, stream
	| _ -> assert false

let bindump_edge x = bindump_block x [] |> Bitv.L.of_bool_list
let binload_edge stream =
	let x, stream = binload_block (Bitv.L.to_bool_list stream) in
	assert(stream = []);
	x

let sub_dummydump sub = "["^(StrUtil.catmap ", " (function
		| S -> "S"
		| P -> "P"
		| X(b, i) -> "X("^(if b then "1" else "0")^", "^(string_of_int i)^")") sub)^" ]"

let block_dummydump block =
	(GUtils.mp_of_bool block.neg)^(GUtils.mp_of_bool block.shift)^(sub_dummydump block.sub)

let edge_dummydump (block, gtree) = "( "^(block_dummydump block)^", "^(match gtree with Utils.Leaf () -> "Leaf" | Utils.Node _ -> "Node")^" )"



let bindump_block2 block stream =
	block.negX::block.negY::block.shiftX::block.shiftY::(BinDump.none_list bindump_elem2 block.subXY stream)

let bindump_node x = bindump_block2 x [] |> Bitv.L.of_bool_list

let binload_block2 = function
	| negX::negY::shiftX::shiftY::stream ->
		let subXY, stream = BinLoad.none_list binload_elem2 stream in
		{negX; negY; shiftX; shiftY; subXY}, stream
	| _ -> assert false

let binload_node stream =
	let x, stream = binload_block2 (Bitv.L.to_bool_list stream) in
	assert(stream = []);
	x

let bindump_tacx (ttag, block) stream =
	let stream = bindump_block2 block stream in
	match ttag with
	| TacxTypes.Cons -> false::false::stream
	| TacxTypes.And  -> false::true ::stream
	| TacxTypes.Xor  -> true ::false::stream

let bindump_tacx x = bindump_tacx x [] |> Bitv.L.of_bool_list

let binload_tacx = function
	| b0::b1::stream ->
	(
		let block, stream = binload_block2 stream in
		((match b0, b1 with
		| false, false -> TacxTypes.Cons
		| false, true  -> TacxTypes.And
		| true , false -> TacxTypes.Xor
		| true , true  -> assert false), block), stream
	)
	| _ -> assert false

let binload_tacx stream =
	let x, stream = binload_tacx (Bitv.L.to_bool_list stream) in
	assert(stream = []);
	x

let block_to_pretty block =
	let _, maxX = classify block in
	let pretty_x iB tB = match iB, tB with
		| false, false -> "1"
		| true , false -> "0"
		| false, true  -> "I"
		| true , true  -> "O"
	in
	let floor = (if block.neg then "-" else "+")^(StrUtil.catmap "" (function S -> "S" | P -> "U" | X(b, 0) -> pretty_x b block.shift | X _ -> ".") block.sub) in
	match maxX with
	| None
	| Some 0 -> floor
	| Some n ->
		let uppers = MyList.init n (fun i -> let i = i+1 in " "^(StrUtil.catmap "" (function X(b, j) -> if i < j then "." else if j = i then (pretty_x b (block.shift <> (mod2 i))) else " " | _ -> " ") block.sub)) in
		String.concat "\n" (List.rev (floor::uppers))
