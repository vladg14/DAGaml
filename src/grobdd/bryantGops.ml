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

let bindump_edge (b, u) (stream:bool list) : bool list =
	BinDump.bool b (bindump_uniq u stream)
let binload_edge stream =
	let b, stream = BinLoad.bool stream in
	let u, stream = binload_uniq stream in
	(b, u), stream

let bindump_node (b, p) stream : bool list = BinDump.bool b (bindump_pair p stream)
let binload_node stream =
	let b, stream = BinLoad.bool stream in
	let p, stream = binload_pair stream in
	(b, p), stream

let bindump_node_and ((b0, b1), p) stream : bool list = BinDump.bool b0 (BinDump.bool b1 (bindump_pair p stream))
let binload_node_and stream =
	let b0, stream = BinLoad.bool stream in
	let b1, stream = BinLoad.bool stream in
	let p, stream = binload_pair stream in
	((b0, b1), p), stream

let bindump_node_xor = bindump_pair
let binload_node_xor = binload_pair

let bindump_tacx = BinDump.pair TacxTypes.bindump_ttag bindump_pair
let binload_tacx = BinLoad.pair TacxTypes.binload_ttag binload_pair


let strdump_edge (b, (i, j)) = (GUtils.mp_of_bool b)^" "^(string_of_int i)^" -> "^(string_of_int j)

let eq getid = function
	| (Utils.Leaf (), Utils.Leaf ()) -> true
	| (Utils.Node nx, Utils.Node ny) -> (getid nx) = (getid ny)
	| _								 -> false

let solve_cons getid (((bX, (xX, yX)) as eX), iX) (((bY, (xY, yY)) as eY), iY) = 
	assert(xX = xY);
	if (eq getid (iX, iY)) && (eX = eY)
	then (Utils.MEdge ((bX, (xX+1, yX)), iX))
	else
	(
		Utils.MNode ((bX, (xX+1, xX+1)), ((bX<>bY, (xX, yX, yY)), iX, iY))
	)

let node_push_cons gid x y = match solve_cons gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (e', x, y)) -> Utils.MNode (e, (((e' |> dump_stream bindump_node):Bitv.t), x, y))

let tacx_push_cons gid x y = match solve_cons gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, ((b, l), x, y)) -> Utils.MNode (e, (((TacxTypes.TCons b, l) |> dump_stream bindump_tacx), x, y))

let get_root b ((_, (xX, _)), _) = ((b, (xX, 0)), Utils.Leaf())
let get_const b (_, (xX, _)) = (b, (xX, 0))

let solve_and gid (((bX, (xX, yX)), iX) as x) (((bY, (xY, yY)), iY) as y)= 
	assert(xX = xY);
	match iX with
	| Utils.Leaf () -> Utils.MEdge (if bX then (* x = 1 *) y else (* x = 0 *) x)
	| Utils.Node nx ->
	match iY with
	| Utils.Leaf () -> Utils.MEdge (if bY then (* y = 1 *) x else (* y = 0 *) y)
	| Utils.Node ny ->
	if (gid nx = gid ny) && (yX = yY)
	then Utils.MEdge (if bX = bY
		then (* x =  y *) x
		else (* x = ~y *) (get_root false x)
		             )
	else
	(
		let yXY = max yX yY in
		Utils.MNode ((false, (xX, yXY)), (((bX, bY), (yXY, yX, yY)), iX, iY))
	)

let tacx_push_and gid x y = match solve_and gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (((bx, by), lxy), x, y)) -> Utils.MNode (e, (((TacxTypes.TAnd (bx, by), lxy) |> dump_stream bindump_tacx), x, y))

let node_solve_and : ('t -> 'i) -> 't edge * 't edge -> ('t edge, edge_state * (Bitv.t * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode)) Utils.merge =
	fun gid (x, y) -> match solve_and gid x y with
		| Utils.MEdge e -> Utils.MEdge e
		| Utils.MNode (e, (e', x, y)) -> Utils.MNode (e, (dump_stream bindump_node_and e', x, y))

let neg ((b, l), i) = ((not b, l), i)
let cneg x ((b, l), i) = ((x <> b, l), i)

let solve_xor gid (((bX, (xX, yX)), iX) as x) (((bY, (xY, yY)), iY) as y)= 
	assert(xX = xY);
	match iX with
	| Utils.Leaf () -> Utils.MEdge (cneg bX y)
	| Utils.Node nx ->
	match iY with
	| Utils.Leaf () -> Utils.MEdge (cneg bY x)
	| Utils.Node ny ->
	if (nx == ny) && (yX = yY)
	then Utils.MEdge (get_root (bX<>bY) x)
	else
	(
		let yXY = max yX yY in
		Utils.MNode ((bX <> bY, (xX, yXY)), ((yXY, yX, yY), iX, iY))
	)

let tacx_push_xor gid x y = match solve_xor gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (l, x, y)) -> Utils.MNode (e, (dump_stream bindump_tacx (TacxTypes.TXor, l), x, y))

let node_solve_xor gid (x, y) = match solve_xor gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (l, x, y)) -> Utils.MNode (e, (dump_stream bindump_node_xor l, x, y))

let tacx_push gid = function
	| TacxTypes.And  -> tacx_push_and  gid
	| TacxTypes.Cons -> tacx_push_cons gid
	| TacxTypes.Xor  -> tacx_push_xor  gid


let compose (bC, (xC, yC)) ((bc, (xc, yc)), i) =
	assert(yC = xc);
	((bC <> bc, (xC, yc)), i)


let split_pair (x, y0, y1) = (x, y0), (x, y1)

let node_split (b, lxy) =
	let lx, ly = split_pair lxy in
	((false, lx), (b, ly))

let node_pull_node _ (c, ix, iy) =
	let ex, ey = node_split (load_stream binload_node c) in
	(ex, ix), (ey, iy)

let node_pull getid ((b, (x, y)), i) =
	if x = y
	then
	(
		Utils.MNode (fun node ->
			let x', y' = node_pull_node getid node in
			(cneg b x', cneg b y')
		)
	)
	else
	(
		let e = ((b, (x-1, y)), i) in
		Utils.MEdge (e, e)
	)

let tacx_split (t, lxy) =
	let lx, ly = split_pair lxy in
	match t with
	| TacxTypes.TAnd (bx, by) -> (TacxTypes.And,  (bx, lx), (by, ly))
	| TacxTypes.TCons by		-> (TacxTypes.Cons, (false, lx), (by, ly))
	| TacxTypes.TXor			-> (TacxTypes.Xor,  (false, lx), (false, ly))

let tacx_pull_node _ (c, ix, iy) =
	let t, ex, ey = tacx_split (load_stream binload_tacx c) in
	(t, (ex, ix), (ey, iy))

let tacx_pull gid edge = assert false
