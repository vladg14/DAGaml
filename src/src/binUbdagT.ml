open Extra
open O3Extra
module type MODELE =
sig
	module M : BinUbdag.MODELE

	val push_node : 'i M.node' -> M.edge * ('i M.next', 'i M.node') Utils.merge

	val compose : M.edge -> 'i M.edge' -> 'i M.edge'

end

module type MODULE_SIG =
sig
	module M : MODELE
	module G : BinUbdag.MODULE_SIG with
		    type M.leaf = M.M.leaf
		and type M.edge = M.M.edge
		and type M.node = M.M.node

	type manager

	val makeman : int -> manager
	val newman : unit -> manager

	val dump_stats : manager -> Tree.stree

	val push : manager -> G.node' -> G.edge'
	val pull : manager -> G.ident -> G.node'

	val dump : manager -> G.edge' list -> Tree.stree
	val load : Tree.stree  -> manager * G.edge' list

	val export : manager -> G.manager
	val import : G.manager -> manager
end

module MODULE(M0:MODELE) =
struct
	module M = M0
	module G = BinUbdag.MODULE(M0.M)

	type manager = G.manager

    let makeman = G.makeman

    let newman  = G.newman

    let dump_stats = G.dump_stats

	let push man node' =
		let edge, merge = M.push_node node' in
		(edge, match merge with
		| Utils.MEdge next' -> next'
		| Utils.MNode node' -> Utils.Node (G.push man node'))
	let pull man ident = G.pull man ident

	let dump = G.dump
	let load = G.load
	
	let export man = man
	let import man = man
	
end

module type PEVAL_MODELE =
sig
	module M : MODULE_SIG

	type peval

	val o3s_peval : peval BinO3.o3s

	type next = peval option * M.G.ident

	type next' = next M.M.M.next'
	type edge' = next M.M.M.edge'
	type node' = next M.M.M.node'

	val eval_edge : peval -> edge' ->  edge'
	val eval_node : peval -> node' -> (edge', node') Utils.merge
	
end

module type PEVAL_SIG =
sig
	module M : PEVAL_MODELE
	
	type manager
	
	val rec_edge : manager -> M.edge' -> M.M.G.edge'
	val rec_node : manager -> M.peval -> M.M.G.ident -> M.M.G.edge'
	val map_edge : manager -> M.peval -> M.M.G.edge' -> M.M.G.edge'
	val map_node : manager -> M.peval -> M.M.G.node' -> M.M.G.edge'

end


module PEVAL(M:PEVAL_MODELE) =
struct
	type manager = {
		man : M.M.manager;
		mem : (M.peval * M.M.G.ident, Bitv.t, M.M.G.edge', Bitv.t) MemoBTable.t;
		rec_edge : M.edge' -> M.M.G.edge';
		rec_node : M.peval -> M.M.G.ident -> M.M.G.edge';
		map_edge : M.peval -> M.M.G.edge' -> M.M.G.edge';
		map_node : M.peval -> M.M.G.node' -> M.M.G.edge';
	}

	let makeman man hsize =
		let o3sA = BinO3.closure M.(o3s_peval +* M.G.o3s_ident) in
		let mem, apply = MemoBTable.make o3sA M.M.G.o3b_edge' hsize in
		let compose = M.M.M.compose in
		let rec rec_edge  (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (edge, Utils.Node ident)
				| Some peval -> compose edge (rec_node peval ident)
		and     rec_node peval ident = apply (fun (peval, ident) ->
			map_node peval (M.M.pull man ident)
		) (peval, ident)
		and     map_node peval node = 
			match M.eval_node peval (Utils.pnode_of_node node) with
			| Utils.MEdge edge -> rec_edge edge
			| Utils.MNode (node, edge0, edge1) ->
				M.M.push man (node, rec_edge edge0, rec_edge edge1)
		in
		let map_edge peval edge = rec_edge (M.eval_edge peval (Utils.pedge_of_edge edge)) in
		{man; mem; rec_edge; rec_node; map_edge; map_node}

    let newman man = makeman man MemoBTable.default_size

	let rec_edge man = man.rec_edge
	let rec_node man = man.rec_node
	let map_edge man = man.map_edge
	let map_node man = man.map_node

end

module PEVAL_CACHED(M:PEVAL_MODELE) =
struct
	type manager = {
		man : M.M.manager;
		mem : (M.peval * M.M.G.ident, Bitv.t, M.M.G.edge', Bitv.t) Hashcache.tt;
		rec_edge : M.edge' -> M.M.G.edge';
		rec_node : M.peval -> M.M.G.ident -> M.M.G.edge';
		map_edge : M.peval -> M.M.G.edge' -> M.M.G.edge';
		map_node : M.peval -> M.M.G.node' -> M.M.G.edge';
	}

	let makeman man hsize =
		let o3sA = BinO3.closure M.(o3s_peval +* M.G.o3s_ident) in
		let mem, apply = Hashcache.tt_make o3sA M.M.G.o3b_edge' hsize in
		let compose = M.M.M.compose in
		let rec rec_edge  (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (edge, Utils.Node ident)
				| Some peval -> compose edge (rec_node peval ident)
		and     rec_node peval ident = apply (fun (peval, ident) ->
			map_node peval (M.M.pull man ident)
		) (peval, ident)
		and     map_node peval node = 
			match M.eval_node peval (Utils.pnode_of_node node) with
			| Utils.MEdge edge -> rec_edge edge
			| Utils.MNode (node, edge0, edge1) ->
				M.M.push man (node, rec_edge edge0, rec_edge edge1)
		in
		let map_edge peval edge = rec_edge (M.eval_edge peval (Utils.pedge_of_edge edge)) in
		{man; mem; rec_edge; rec_node; map_edge; map_node}

    let newman man = makeman man Hashcache.default_size

	let rec_edge man = man.rec_edge
	let rec_node man = man.rec_node
	let map_edge man = man.map_edge
	let map_node man = man.map_node

end

module type REWRITE_MODELE =
sig
	module M : PEVAL_SIG

	type leaf = M.M.M.M.M.leaf
	type edge = M.M.M.M.M.edge
	type node = M.M.M.M.M.node

	type ident = M.M.M.G.ident
	type next' = M.M.M.G.next'
	type edge' = M.M.M.G.edge'
	type node' = M.M.M.G.node'

	type next'' = M.M.next'
	type edge'' = M.M.edge'
	type node'' = M.M.node'
	type tree'' = (next'', edge, node) GTree.edge

	val rewrite : node'' -> tree''
end

module REWRITE(M0:REWRITE_MODELE) =
struct

	type manager = {
		man : M0.M.M.M.manager;
		memR : (M0.ident, M0.ident, M0.edge', Bitv.t) MemoBTable.t;
		memE : (M0.M.M.peval * M0.ident, Bitv.t, M0.edge', Bitv.t) MemoBTable.t;
		map : M0.M.M.peval option -> M0.edge' -> M0.edge';
	}

	let makeman man hsize =
		let o3sB = M0.M.M.M.G.o3b_edge' in
		let memR, applyR = MemoBTable.make O3.id o3sB hsize in
		let o3sA = BinO3.closure M0.M.M.(o3s_peval +* M.G.o3s_ident) in
		let memE, applyE = MemoBTable.make o3sA  o3sB hsize in
		let compose = M0.M.M.M.M.compose in	
		let push : M0.node' -> M0.edge' = M0.M.M.M.push man
		and pull : M0.ident -> M0.node' = M0.M.M.M.pull man in
		(* goup = go up *)
		let rec goup_edge ((edge, next) : M0.edge'') : bool * M0.edge'= match next with
			| Utils.Leaf leaf -> (false, (edge, Utils.Leaf leaf))
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (false, (edge, Utils.Node ident))
				| Some peval -> (true, compose edge (eval_ident peval ident))
		and     eval_ident peval ident : M0.edge' = applyE (fun (peval, ident) ->
			let node = pull ident in
			match M0.M.M.eval_node peval (Utils.pnode_of_node node) with
			| Utils.MEdge edge -> goup_edge edge |> snd
			| Utils.MNode node -> goup_node node
		) (peval, ident)
		and     goup_node (node, edge0, edge1) : M0.edge' =
			let t0, edge0 = goup_edge edge0
			and t1, edge1 = goup_edge edge1 in
			let node : M0.node' = (node, edge0, edge1) in
			if t0||t1
			then (rewr node)
			else (push node)
		and     rewr (node : M0.node') : M0.edge' = read_edge (M0.rewrite (Utils.pnode_of_node node))
		and     read_edge (edge, node) : M0.edge' = match node with
			| GTree.Leaf (next : M0.next'') -> goup_edge ((edge, next) : M0.edge'') |> snd
			| GTree.Node (node, edge0, edge1) ->
				goup_node (Utils.pnode_of_node (node, read_edge edge0, read_edge edge1))
		in
		let rec down_ident ident : M0.edge' = applyR (fun ident ->
			let (node, edge0, edge1) = pull ident in
			rewr (node, down_edge edge0, down_edge edge1)
		) ident
		and     down_edge ((edge, next) : M0.edge') : M0.edge' = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node ident -> compose edge (down_ident ident)
		in
		let map opeval edge : M0.edge' =
			let edge = match opeval with
				| None -> edge
				| Some peval -> goup_edge (M0.M.M.eval_edge peval (Utils.pedge_of_edge edge)) |> snd
			in
			down_edge edge
		in
		{man; memR; memE; map}
	
end

module REWRITE_CACHED(M0:REWRITE_MODELE) =
struct

	type manager = {
		man : M0.M.M.M.manager;
		memR : (M0.ident, M0.ident, M0.edge', Bitv.t) Hashcache.tt;
		memE : (M0.M.M.peval * M0.ident, Bitv.t, M0.edge', Bitv.t) Hashcache.tt;
		map : M0.M.M.peval option -> M0.edge' -> M0.edge';
	}

	let makeman man hsize =
		let o3sB = M0.M.M.M.G.o3b_edge' in
		let memR, applyR = Hashcache.tt_make O3.id o3sB hsize in
		let o3sA = BinO3.closure M0.M.M.(o3s_peval +* M.G.o3s_ident) in
		let memE, applyE = Hashcache.tt_make o3sA  o3sB hsize in
		let compose = M0.M.M.M.M.compose in	
		let push : M0.node' -> M0.edge' = M0.M.M.M.push man
		and pull : M0.ident -> M0.node' = M0.M.M.M.pull man in
		(* goup = go up *)
		let rec goup_edge ((edge, next) : M0.edge'') : bool * M0.edge'= match next with
			| Utils.Leaf leaf -> (false, (edge, Utils.Leaf leaf))
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (false, (edge, Utils.Node ident))
				| Some peval -> (true, compose edge (eval_ident peval ident))
		and     eval_ident peval ident : M0.edge' = applyE (fun (peval, ident) ->
			let node = pull ident in
			match M0.M.M.eval_node peval (Utils.pnode_of_node node) with
			| Utils.MEdge edge -> goup_edge edge |> snd
			| Utils.MNode node -> goup_node node
		) (peval, ident)
		and     goup_node (node, edge0, edge1) : M0.edge' =
			let t0, edge0 = goup_edge edge0
			and t1, edge1 = goup_edge edge1 in
			let node : M0.node' = (node, edge0, edge1) in
			if t0||t1
			then (rewr node)
			else (push node)
		and     rewr (node : M0.node') : M0.edge' = read_edge (M0.rewrite (Utils.pnode_of_node node))
		and     read_edge (edge, node) : M0.edge' = match node with
			| GTree.Leaf (next : M0.next'') -> goup_edge ((edge, next) : M0.edge'') |> snd
			| GTree.Node (node, edge0, edge1) ->
				goup_node (Utils.pnode_of_node (node, read_edge edge0, read_edge edge1))
		in
		let rec down_ident ident : M0.edge' = applyR (fun ident ->
			let (node, edge0, edge1) = pull ident in
			rewr (node, down_edge edge0, down_edge edge1)
		) ident
		and     down_edge ((edge, next) : M0.edge') : M0.edge' = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node ident -> compose edge (down_ident ident)
		in
		let map opeval edge : M0.edge' =
			let edge = match opeval with
				| None -> edge
				| Some peval -> goup_edge (M0.M.M.eval_edge peval (Utils.pedge_of_edge edge)) |> snd
			in
			down_edge edge
		in
		{man; memR; memE; map}
	
end

module IMPORT(M0:MODULE_SIG) =
struct
	module G = M0

	module MODELE =
	struct
		module M = G.G

		type extra  = G.manager
		type xnode  = M.edge'
		type xnode' = Bitv.t
		type xedge  = M.edge'

		let o3_xnode = M.o3b_edge'

		let rec_edge (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node node -> G.M.compose edge (node())

		let map_node man (node, edge0, edge1) =
			G.push man (node, rec_edge edge0, rec_edge edge1)

		let map_edge man edge = rec_edge edge
		
	end

	include BinUbdag.EXPORT(MODELE)

end

module STDIO(M1:MODELE) =
struct
	module G1 = MODULE(M1)
	module G0 = G1.G

	module REMAN = BinUbdag.REMAN(G0)
	module IMPORT = IMPORT(G1)

	let stree_dump man edges =
		let ubdag = G1.export man in
		let ubdag' = G0.newman () in
		let man = REMAN.newman ubdag ubdag' in
		let map = REMAN.map_edge man in
		let edges' = List.map map edges in
		G0.dump ubdag' edges'

	let stree_load stree =
		let ubdag', edges' = G0.load stree in
		let grobdd = G1.newman () in
		let man = IMPORT.newman ubdag' grobdd in
		let map = IMPORT.rec_edge man in
		let edges = List.map map edges' in
		(grobdd, edges)

	let dumpfile man edges target =
		STree.dumpfile [stree_dump man edges] target
	
	let loadfile target =
		match STree.loadfile target with
			| [] -> assert false
			| objet::_ -> stree_load objet
end
