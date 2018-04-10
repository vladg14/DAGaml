let cons_is_reversible push pull arity (((), edge0, edge1) as node) =
	assert(arity edge0 = arity edge1);
	let edge01, merge = push node in
	match merge with
	| Utils.MEdge next01 ->
	(
		assert(arity edge0 + 1 = arity (edge01, next01));
		match pull (edge01, next01) with
			| Utils.MEdge node''  -> assert(node = node'')
			| Utils.MNode _       -> assert false
	)
	| Utils.MNode (((), edge0', edge1') as node') ->
	(
		assert(arity edge0' = arity edge1');
		match pull (edge01, Utils.Node (None, 0)) with
			| Utils.MEdge _       -> assert false
			| Utils.MNode (_, f) ->
			(
				let node'' = f node' in
				assert(node = node'')
			)
	);
	()
