open Extra
open O3Extra

module type MODELE =
sig
	module M : BinUbdag.MODELE

	type peval

	val o3s_peval : peval BinO3.o3s

	type 'i link = peval option * 'i
	type 'i next'' = 'i link M.next'
	type 'i edge'' = 'i link M.edge'
	type 'i node'' = 'i link M.node'
	type 'i tree'' = ('i next'', M.edge, M.node) GTree.edge

	val o3s_next'' : ('i BinO3.o3s) -> 'i next'' BinO3.o3s
	val o3s_edge'' : ('i BinO3.o3s) -> 'i edge'' BinO3.o3s
	val o3s_node'' : ('i BinO3.o3s) -> 'i node'' BinO3.o3s
	val o3s_tree'' : ('i BinO3.o3s) -> 'i tree'' BinO3.o3s
	
	val eval_edge : peval -> 'i edge'' ->  'i edge''
	val eval_node : peval -> 'i node'' -> ('i edge'', 'i node'') Utils.merge

	val push_node : 'i node'' -> 'i tree''

	val compose : M.edge -> 'i M.edge' -> 'i M.edge'

end

module type MODULE_SIG =
sig
	module M : MODELE
	module G : BinUbdag.MODULE_SIG with
		    type M.leaf = M.M.leaf
		and type M.edge = M.M.edge
		and type M.node = M.M.node

	type link = M.peval option * G.ident
	type next'' = G.ident M.next''
	type edge'' = G.ident M.edge''
	type node'' = G.ident M.node''
	type tree'' = G.ident M.tree''

	type manager

	val makeman : int -> manager
	val newman : unit -> manager

	val dump_stats : manager -> StrTree.tree

	val push : manager -> G.node' -> G.edge'
	val pull : manager -> G.ident -> G.node'

	val dump : manager -> G.edge' list -> StrTree.tree
	val load : StrTree.tree  -> manager * G.edge' list

	val eval : manager -> M.peval -> G.edge' -> G.edge'

	val export : manager -> G.manager
	val import : G.manager -> manager
end

module MODULE(M0:MODELE) =
struct
	module M = M0
	module G = BinUbdag.MODULE(M0.M)

	type link = M.peval option * G.ident
	type next'' = G.ident M.next''
	type edge'' = G.ident M.edge''
	type node'' = G.ident M.node''
	type tree'' = G.ident M.tree''

	let o3s_next'' = M.o3s_next'' G.o3s_ident
	let o3s_edge'' = M.o3s_edge'' G.o3s_ident
	let o3s_node'' = M.o3s_node'' G.o3s_ident
	let o3s_tree'' = M.o3s_tree'' G.o3s_ident

    type manager = {
		man : G.manager;
		mem : (M.peval * G.ident, Bitv.t, G.edge', Bitv.t) MemoBTable.t;
		eval : M.peval -> G.edge' -> G.edge';
		push : G.node' -> G.edge';
	}

	let export man = man.man

	let import' hsize man =
		let o3sA = BinO3.closure (M.o3s_peval +* G.o3s_ident) in
		let mem, apply = MemoBTable.make o3sA G.o3b_edge' hsize in
		(*mem.MemoBTable.each <- (fun a aa b bb ->
			print_string "##";
			print_string (Bitv.L.to_hexa_string aa);
			print_string " ";
			print_string (Bitv.L.to_hexa_string bb);
			print_newline()
		);*)
		let rec push node : G.edge' = read_edge (M.push_node (Utils.pnode_of_node node))
		and     eval peval (edge : G.edge') : G.edge' =
			goup_edge' (M.eval_edge peval (Utils.pedge_of_edge edge))
		and     goup_edge ((edge, next) : edge'') : bool * G.edge' = match next with
			| Utils.Leaf leaf -> (false, (edge, Utils.Leaf leaf))
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (false, (edge, Utils.Node ident))
				| Some peval -> (true, M.compose edge (eval_ident peval ident))
		and     goup_edge' edge : G.edge' = goup_edge edge |> snd
		and     eval_ident peval ident = apply (fun (peval, ident) ->
			let node = G.pull man ident |> Utils.pnode_of_node in
			match M.eval_node peval node with
			| Utils.MEdge edge -> goup_edge' edge
			| Utils.MNode (node, edge0, edge1) ->
				push (node, goup_edge' edge0, goup_edge' edge1)
		) (peval, ident)
		and     goup_node edge (node, edge0, edge1) =
			let t0, edge0 = goup_edge edge0
			and t1, edge1 = goup_edge edge1 in
			let node = (node, edge0, edge1) in
			if t0||t1
			then (M.compose edge (push node))
			else (edge, Utils.Node (G.push man node))
		and     read_edge (edge, next) = match next with
			| GTree.Leaf next -> goup_edge' (edge, next)
			| GTree.Node node -> goup_node   edge (read_node node |> Utils.pnode_of_node)
		and     read_node (node, edge0, edge1) =
			(node, read_edge edge0, read_edge edge1)
		in
		{man; mem; push; eval}

	let makeman hsize =
		let man = G.makeman hsize in
		import' hsize man

    let default_newman_hsize = 10000

    let newman () = makeman default_newman_hsize

	let import = import' default_newman_hsize

    let dump_stats man = Tree.Node [
		G.dump_stats man.man;
		MemoBTable.dump_stats man.mem;
	]

	let push man = man.push
	let pull man ident = G.pull man.man ident

	let dump man = G.dump man.man
	let load stree =
		let man, edges = G.load stree in
		(import man, edges)

	let eval man = man.eval


	
end

module type REWRITE_MODELE =
sig
	module M : MODULE_SIG

	type leaf = M.M.M.leaf
	type edge = M.M.M.edge
	type node = M.M.M.node

	type ident = M.G.ident
	type next' = M.G.next'
	type edge' = M.G.edge'
	type node' = M.G.node'

	type next'' = M.next''
	type edge'' = M.edge''
	type node'' = M.node''
	type tree'' = M.tree''

	val rewrite : node'' -> tree''
end

module REWRITE(M0:REWRITE_MODELE) =
struct

	type manager = {
		man : M0.M.manager;
		memR : (M0.ident, M0.ident, M0.edge', Bitv.t) MemoBTable.t;
		memE : (M0.M.M.peval * M0.ident, Bitv.t, M0.edge', Bitv.t) MemoBTable.t;
		map : M0.M.M.peval option -> M0.edge' -> M0.edge';
	}

	let makeman man hsize =
		let o3sB = M0.M.G.o3b_edge' in
		let memR, applyR = MemoBTable.make O3.id o3sB hsize in
		let o3sA = BinO3.closure M0.M.(M.o3s_peval +* G.o3s_ident) in
		let memE, applyE = MemoBTable.make  o3sA o3sB hsize in
		let compose = M0.M.M.compose in	
		let push : M0.node' -> M0.edge' = M0.M.push man
		and pull : M0.ident -> M0.node' = M0.M.pull man in
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

		type next' = (unit -> xnode) M.M.next'
		type edge' = (unit -> xnode) M.M.edge'
		type node' = (unit -> xnode) M.M.node'

		let rec_edge (edge, next) : M.edge' = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node node -> G.M.compose edge (node())

		let map_node man (node, edge0, edge1) : M.edge' =
			G.push man (node, rec_edge edge0, rec_edge edge1)

		let map_edge man edge : M.edge' = rec_edge edge
		
	end

	include BinUbdag.EXPORT(MODELE)

end

module STDIO(M1:MODELE) =
struct
	module G1 = MODULE(M1)
	module G0 = G1.G

	module REMAN = BinUbdag.REMAN(G0)

	let dumpfile man edges target =
		let ubdag = G1.export man in
		let ubdag' = G0.newman () in
		let man = REMAN.newman ubdag ubdag' in
		let map = REMAN.map_edge man in
		let edges' = List.map map edges in
		let stree = G0.dump ubdag' edges' in
		StrTree.dumpfile [stree] target

	module IMPORT = IMPORT(G1)
	
	let loadfile target =
		let ubdag', edges' = match StrTree.loadfile target with
			| [objet] -> G0.load objet
			| _ -> assert false
		in
		let grobdd = G1.newman () in
		let man = IMPORT.newman ubdag' grobdd in
		let map = IMPORT.rec_edge man in
		let edges = List.map map edges' in
		(grobdd, edges)
end
