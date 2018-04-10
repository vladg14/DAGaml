(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open BryantTypes
open Extra

let arity ((_, (n, _)), _) = n

let bindump_uniq = BinDump.(pair int int)
let binload_uniq = BinLoad.(pair int int)

let bindump_pair = BinDump.(trio int int int)
let binload_pair = BinLoad.(trio int int int)

let bindump_edge = BinDump.(pair bool bindump_uniq)
let binload_edge = BinLoad.(pair bool binload_uniq)

let bindump_node = BinDump.(trio unit bindump_edge bindump_edge)
let binload_node = BinLoad.(trio unit binload_edge binload_edge)

let bindump_tacx = BinDump.trio TacxTypes.bindump_tag bindump_edge bindump_edge
let binload_tacx = BinLoad.trio TacxTypes.binload_tag binload_edge binload_edge

let strdump_edge = StrDump.(pair bool (pair int int))
let pretty_edge (b, (i, j)) = (GUtils.mp_of_bool b)^" "^(string_of_int i)^" -> "^(string_of_int j)

let solve_cons ((), (((bX, (xX, yX)), iX) as edgeX), (((bY, (xY, yY)), iY) as edgeY)) =
	assert(xX = xY);
	if edgeX = edgeY
	then (Utils.MEdge ((bX, (xX+1, yX)), iX))
	else
	(Utils.MNode (
		(bX, (xX+1, xX+1)),
		((), ((false, (xX, yX)), iX), ((bX<>bY, (xY, yY)), iY))
	))

let neg ((b, l), i) = ((not b, l), i)
let cneg x ((b, l), i) = ((x <> b, l), i)

let get_root b ((_, (x, _)), _) = ((b, (x, 0)), Utils.Leaf())

let solve_and ((), (((bX, (xX, yX)), iX) as edgeX), (((bY, (xY, yY)), iY) as edgeY)) = 
	assert(xX = xY);
	match iX with
	| Utils.Leaf () -> Utils.MEdge (if bX then (* x = 1 *) edgeY else (* x = 0 *) edgeX)
	| Utils.Node nx ->
	match iY with
	| Utils.Leaf () -> Utils.MEdge (if bY then (* y = 1 *) edgeX else (* y = 0 *) edgeY)
	| Utils.Node ny ->
	if (nx = ny) && (yX = yY)
	then Utils.MEdge (if bX = bY
		then (* x =  y *) edgeX
		else (* x = ~y *) (get_root false edgeX)
		             )
	else
	(
		let yXY = max yX yY in Utils.MNode (
			(false, (xX, yXY)),
			((), ((bX, (yXY, yX)), iX), ((bY, (yXY, yY)), iY))
		)
	)


let solve_xor ((), (((bX, (xX, yX)), iX) as edgeX), (((bY, (xY, yY)), iY) as edgeY)) =
	assert(xX = xY);
	match iX with
	| Utils.Leaf () -> Utils.MEdge (cneg bX edgeY)
	| Utils.Node nx ->
	match iY with
	| Utils.Leaf () -> Utils.MEdge (cneg bY edgeX)
	| Utils.Node ny ->
	if (nx = ny) && (yX = yY)
	then Utils.MEdge (get_root (bX<>bY) edgeX)
	else
	(
		let yXY = max yX yY in Utils.MNode (
			(bX<>bY, (xX, yXY)),
			((), ((false, (yXY, yX)), iX), ((false, (yXY, yY)), iY))
		)
	)

let solve_node node = match solve_cons node with
	| Utils.MEdge (edge, next) -> edge, Utils.MEdge next
	| Utils.MNode (edge, node) -> edge, Utils.MNode node

let cons_and  node = match solve_and node with
	| Utils.MEdge (edge, next) -> edge, Utils.M3Edge next
	| Utils.MNode (edge, node) -> edge, Utils.M3Node node

let cons_xor  node = match solve_xor node with
	| Utils.MEdge (edge, next) -> edge, Utils.M3Edge next
	| Utils.MNode (edge, node) -> edge, Utils.M3Node node

let solve_of_tag = TacxTypes.(function
	| And  -> solve_and
	| Cons -> solve_cons
	| Xor  -> solve_xor)

let solve_tacx (tag, edge0, edge1) = match (solve_of_tag tag) ((), edge0, edge1) with
	| Utils.MEdge (edge, next) -> edge, Utils.MEdge next
	| Utils.MNode (edge, ((), edge0, edge1)) -> edge, Utils.MNode (tag, edge0, edge1)

let compose (bC, (xC, yC)) ((bc, (xc, yc)), i) =
	assert(yC = xc);
	((bC <> bc, (xC, yc)), i)


let node_pull ((b, (x, y)), i) =
	assert(x>=y);
	if x = y
	then
	(
		Utils.MNode (Utils.gnode_node i, fun ((), x', y') ->
			((), cneg b x', cneg b y')
		)
	)
	else
	(
		let e = ((b, (x-1, y)), i) in
		Utils.MEdge ((), e, e)
	)
