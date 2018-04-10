(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open BryantTypes
open Extra

let dump_stream f x = (f x []) |> Bitv.L.of_bool_list
let load_stream f stream =
	let x, stream = f (stream |> Bitv.L.to_bool_list)in
	assert(stream = []);
	x

let bindump_uniq (i, j) (stream:bool list) : bool list = BinDump.int i (BinDump.int j stream)
let binload_uniq stream =
	let i, stream = BinLoad.int stream in
	let j, stream = BinLoad.int stream in
	(i, j), stream

let bindump_pair (i, j0, j1) stream : bool list = BinDump.int i (BinDump.int j0 (BinDump.int j1 stream))
let binload_pair stream =
	let i, stream  = BinLoad.int stream in
	let j0, stream = BinLoad.int stream in
	let j1, stream = BinLoad.int stream in
	(i, j0, j1), stream

let bindump_edge = bindump_uniq
let binload_edge = binload_uniq

let bindump_node = bindump_pair
let binload_node = binload_pair

let strdump_edge (i, j) = (string_of_int i)^" -> "^(string_of_int j)

let eq getid = function
	| (Utils.Leaf (), Utils.Leaf ()) -> true
	| (Utils.Node nx, Utils.Node ny) -> (getid nx) = (getid ny)
	| _								 -> false

let solve_cons getid ((xX, yX), iX) ((xY, yY), iY) =
	assert(xX = xY);
	match iY with
	| Utils.Leaf false ->
	(
		assert(yY = 0);
		Utils.MEdge ((xX+1, yX), iX)
	)
	| _ ->
	(
		Utils.MNode ((xX+1, xX+1), ((xX, yX, yY), iX, iY))
	)

let node_push_cons gid x y = match solve_cons gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (e', x, y)) -> Utils.MNode (e, (((e' |> dump_stream bindump_node):Bitv.t), x, y))

let get_root b ((x, _), _) = ((x, 0), Utils.Leaf b)
let get_cons   ((x, _), _) = (x, 0)

let compose (xC, yC) ((xc, yc), i) =
	assert(yC = xc);
	((xC, yc), i)


let split_pair (x, y0, y1) = (x, y0), (x, y1)

let node_split = split_pair

let node_pull_node _ (c, ix, iy) =
	let ex, ey = node_split (load_stream binload_node c) in
	(ex, ix), (ey, iy)

let node_pull getid ((x, y), i) =
	if x = y
	then
	(
		Utils.MNode (node_pull_node getid)
	)
	else
	(
		let zero = ((x-1, 0), Utils.Leaf false)
		and path = ((x-1, y), i) in
		Utils.MEdge (path, zero)
	)
