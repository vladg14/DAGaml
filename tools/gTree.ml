type ('leaf, 'edge, 'node) next =
	| Node of ('leaf, 'edge, 'node) node
	| Leaf of 'leaf
and  ('leaf, 'edge, 'node) edge =
	'edge * ('leaf, 'edge, 'node) next
and  ('leaf, 'edge, 'node) node =
	'node * ('leaf, 'edge, 'node) edge * ('leaf, 'edge, 'node) edge

let bindump
	(dump_leaf : 'leaf BinUtils.dump)
	(dump_edge : 'edge BinUtils.dump)
	(dump_node : 'node BinUtils.dump) =
	let rec dump_next' next stream = match next with
		| Node node -> false::(dump_node' node stream)
		| Leaf leaf -> true ::(dump_leaf  leaf stream)
	and     dump_edge' (edge, next) stream =
		dump_edge edge (dump_next' next stream)
	and     dump_node' (node, edge0, edge1) stream =
		dump_node node (dump_edge' edge0 (dump_edge' edge1 stream))
	in
	(dump_next', dump_edge', dump_node')

let bindump_edge leaf edge node =
	let _, edge, _ = bindump leaf edge node in
	edge

let binload
	(load_leaf : 'leaf BinUtils.load)
	(load_edge : 'edge BinUtils.load)
	(load_node : 'node BinUtils.load) =
	let rec load_next' = function
		| false::stream ->
		(
			let node, stream = load_node' stream in
			((Node node), stream)
		)
		| true ::stream ->
		(
			let leaf, stream = load_leaf  stream in
			((Leaf leaf), stream)
		)
		| [] -> assert false
	and     load_edge' stream =
		BinLoad.pair load_edge load_next' stream
	and     load_node' stream =
		let ((node, edge0), edge1), stream = BinLoad.pair (BinLoad.pair load_node load_edge') load_edge' stream in
		(node, edge0, edge1), stream
	in
	(load_next', load_edge', load_node')

let o3s (dump_leaf, load_leaf) (dump_edge, load_edge) (dump_node, load_node) =
	let dump_next', dump_edge', dump_node' = bindump dump_leaf dump_edge dump_node
	and load_next', load_edge', load_node' = binload load_leaf load_edge load_node in
	((dump_next', load_next'), (dump_edge', load_edge'), (dump_node', load_node'))

let binload_edge leaf edge node =
	let _, edge, _ = binload leaf edge node in
	edge

let o3s_edge leaf edge node =
	let _, edge, _ = o3s leaf edge node in
	edge
