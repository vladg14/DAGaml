(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open BryantTypes
open Extra

let arity ((n, _), _) = n

let strdump_uniq = StrDump.(pair int int)
let bindump_uniq = BinDump.(pair int int)
let binload_uniq = BinLoad.(pair int int)

let strdump_pair = StrDump.(trio int int int)
let bindump_pair = BinDump.(trio int int int)
let binload_pair = BinLoad.(trio int int int)

let strdump_edge = strdump_uniq
let bindump_edge = bindump_uniq
let binload_edge = binload_uniq

let bindump_node = BinDump.(trio unit bindump_edge bindump_edge)
let binload_node = BinLoad.(trio unit binload_edge binload_edge)

let strdump = StrDump.(pair int int)
let pretty_edge (i, j) = (string_of_int i)^" -> "^(string_of_int j)

let solve_node ((), (((xX, yX), iX) as edgeX), (((xY, yY), iY) as edgeY)) =
	assert(xX = xY);
	match iY with
	| Utils.Leaf false ->
	(
		assert(yY = 0);
		((xX+1, yX), Utils.MEdge iX)
	)	
	| _ ->
		((xX+1, xX+1), Utils.MNode ((), edgeX, edgeY))


let get_root b ((x, _), _) = ((x, 0), Utils.Leaf b)
let get_cons   ((x, _), _) = (x, 0)

let compose (xC, yC) ((xc, yc), i) =
	assert(yC = xc);
	((xC, yc), i)

let node_pull ((x, y), i) =
	assert(x>=y);
	if x = y
	then
	(
		Utils.MNode (Utils.gnode_node i, fun node -> node)
	)
	else
	(
		let zero = ((x-1, 0), Utils.Leaf false)
		and path = ((x-1, y), i) in
		Utils.MEdge ((), path, zero)
	)
