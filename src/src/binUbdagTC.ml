open Extra
open O3Extra

module type MODELE =
sig
	module M : BinUbdag.MODELE with
		type node = unit

	val arity : 'i M.edge' -> int

	val push_node : 'i M.node' -> M.edge * ('i M.next', 'i M.node') Utils.merge
	val pull_node : 'i M.edge' -> ('i M.node', 'i * ('i M.node' -> 'i M.node')) Utils.merge

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
	val pull : manager -> G.edge' -> G.node'

	val export : manager -> G.manager
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
		let edge, merge = M0.push_node node' in
		(edge, match merge with
		| Utils.MEdge next' -> next'
		| Utils.MNode node' -> Utils.Node (G.push man node'))
	let pull man edge' =
		match M0.pull_node edge' with
		| Utils.MEdge node' -> node'
		| Utils.MNode (ident, func) -> func (G.pull man ident)

	let dump = G.dump
	let load = G.load

	let export man = man
	
end

module type BINOP_MODELE =
sig
	module M : MODULE_SIG
	
	val solver : M.G.node' -> M.M.M.edge * (M.G.next', M.G.node', M.G.node') Utils.merge3

end

module BINOP(M0:BINOP_MODELE) =
struct
	
	module M = M0

	type manager = {
		man : M0.M.manager;
		mem : (M0.M.G.node', Bitv.t, M0.M.G.edge', Bitv.t) MemoBTable.t;
		map : M0.M.G.node' -> M0.M.G.edge';
	}

	let makeman man hsize =
		let mem, apply = MemoBTable.make M0.M.G.o3b_node' M0.M.G.o3b_edge' hsize in
		let push = M0.M.push man
		and pull = M0.M.pull man
		and compose = M0.M.M.compose in
		let rec calcrec ((), edge'X, edge'Y) =
			let edge, merge3 = M0.solver ((), edge'X, edge'Y) in
			match merge3 with
			| Utils.M3Edge next' -> (edge, next')
			| Utils.M3Cons node' -> compose edge (push node')
			| Utils.M3Node node' -> compose edge (apply (fun ((), edgeX, edgeY) ->
				let ((), edgeX0, edgeX1) = pull edgeX
				and ((), edgeY0, edgeY1) = pull edgeY in
				let edge'0 = calcrec ((), edgeX0, edgeY0)
				and edge'1 = calcrec ((), edgeX1, edgeY1) in
				push ((), edge'0, edge'1) ) node')
		in {man; mem; map = calcrec}

    let newman man = makeman man MemoBTable.default_size

    let dump_stats man = MemoBTable.dump_stats man.mem

	let map man = man.map
	
end

module BINOP_CACHED(M0:BINOP_MODELE) =
struct
	
	module M = M0

	type manager = {
		man : M0.M.manager;
		mem : (M0.M.G.node', Bitv.t, M0.M.G.edge', Bitv.t) Hashcache.tt;
		map : M0.M.G.node' -> M0.M.G.edge';
	}

	let makeman man hsize =
		let mem, apply = Hashcache.tt_make M0.M.G.o3b_node' M0.M.G.o3b_edge' hsize in
		let push = M0.M.push man
		and pull = M0.M.pull man
		and compose = M0.M.M.compose in
		let rec calcrec ((), edge'X, edge'Y) =
			let edge, merge3 = M0.solver ((), edge'X, edge'Y) in
			match merge3 with
			| Utils.M3Edge next' -> (edge, next')
			| Utils.M3Cons node' -> compose edge (push node')
			| Utils.M3Node node' -> compose edge (apply (fun ((), edgeX, edgeY) ->
				let ((), edgeX0, edgeX1) = pull edgeX
				and ((), edgeY0, edgeY1) = pull edgeY in
				let edge'0 = calcrec ((), edgeX0, edgeY0)
				and edge'1 = calcrec ((), edgeX1, edgeY1) in
				push ((), edge'0, edge'1) ) node')
		in {man; mem; map = calcrec}

    let newman man = makeman man Hashcache.default_size

    let dump_stats man = Hashcache.tt_dump_stats man.mem

	let map man = man.map
	
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

	val dump_stats : manager -> Tree.stree

end

module PEVAL(M0:PEVAL_MODELE) =
struct
	module M = M0

	type manager = {
		man : M.M.manager;
		mem : (M.peval * M.M.G.ident, Bitv.t, M.M.G.edge', Bitv.t) MemoBTable.t;
		rec_edge : M.edge' -> M.M.G.edge';
		rec_node : M.peval -> M.M.G.ident -> M.M.G.edge';
		map_edge : M.peval -> M.M.G.edge' -> M.M.G.edge';
		map_node : M.peval -> M.M.G.node' -> M.M.G.edge';
	}

	let dump_stats man = MemoBTable.dump_stats man.mem

	let makeman man hsize =
		let o3sA = BinO3.closure (M.o3s_peval +* M.M.G.o3s_ident)
		and o3sB = M.M.G.o3b_edge' in
		let mem, apply = MemoBTable.make o3sA o3sB hsize in
		let compose = M.M.M.compose in
		let pull =
			let man = M.M.export man in
			M.M.G.pull man
		in
		let rec rec_edge  (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (edge, Utils.Node ident)
				| Some peval -> compose edge (rec_node peval ident)
		and     rec_node peval ident = apply (fun (peval, ident) ->
			map_node peval (pull ident)
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

module PEVAL_CACHED(M0:PEVAL_MODELE) =
struct
	module M = M0

	type manager = {
		man : M.M.manager;
		mem : (M.peval * M.M.G.ident, Bitv.t, M.M.G.edge', Bitv.t) Hashcache.tt;
		rec_edge : M.edge' -> M.M.G.edge';
		rec_node : M.peval -> M.M.G.ident -> M.M.G.edge';
		map_edge : M.peval -> M.M.G.edge' -> M.M.G.edge';
		map_node : M.peval -> M.M.G.node' -> M.M.G.edge';
	}

	let dump_stats man = Hashcache.tt_dump_stats man.mem

	let makeman man hsize =
		let o3sA = BinO3.closure (M.o3s_peval +* M.M.G.o3s_ident)
		and o3sB = M.M.G.o3b_edge' in
		let mem, apply = Hashcache.tt_make o3sA o3sB hsize in
		let compose = M.M.M.compose in
		let pull =
			let man = M.M.export man in
			M.M.G.pull man
		in
		let rec rec_edge  (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (edge, Utils.Node ident)
				| Some peval -> compose edge (rec_node peval ident)
		and     rec_node peval ident = apply (fun (peval, ident) ->
			map_node peval (pull ident)
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

module type EBINOP_MODELE =
sig
	module M : PEVAL_SIG

	val solver : M.M.node' -> M.M.M.M.M.edge * (M.M.next', M.M.node', M.M.node') Utils.merge3
end

module EBINOP(M0:EBINOP_MODELE) =
struct
	module M = M0

	type manager = {
		man : M.M.M.M.manager;
		eva : M.M.manager;
		mem : (M.M.M.M.G.node', Bitv.t, M.M.M.M.G.edge', Bitv.t) MemoBTable.t;
		map : M.M.M.M.G.node' -> M.M.M.M.G.edge';
	}

	let makeman man eva hsize =
		let mem, apply = M.M.M.M.G.(MemoBTable.make o3b_node' o3b_edge') hsize in
		let compose = M.M.M.M.M.compose
		and push = M.M.M.M.push man
		and pull = M.M.M.M.pull man
		and eval = M.M.rec_node eva in
		let rec eva_edge (edge, next) = match next with
			| Utils.Leaf leaf -> (false, (edge, Utils.Leaf leaf))
			| Utils.Node(None, node) -> (false, (edge, Utils.Node node))
			| Utils.Node(Some peval, node) -> (true, compose edge (eval peval node))
		and     eva_edge' edge = eva_edge edge |> snd
		and     eva_node' (node, edge0, edge1) =
			(node, eva_edge' edge0, eva_edge' edge1)
		and     eva_node (node, edge0, edge1) =
			let t0, edge0 = eva_edge edge0
			and t1, edge1 = eva_edge edge1 in
			(t0||t1, (node, edge0, edge1))
		and     rec_node node =
			let edge, merge = M.solver(Utils.pnode_of_node node) in
			match merge with
			| Utils.M3Edge next -> eva_edge' (edge, next)
			| Utils.M3Cons node -> compose edge (push(eva_node' node))
			| Utils.M3Node node ->
			let t, node = eva_node node in
			compose edge (if t then (rec_node node) else 
			(apply (fun ((), x, y) -> (
				let ((), x0, x1) = pull x
				and ((), y0, y1) = pull y in
				let xy0 = rec_node ((), x0, y0)
				and xy1 = rec_node ((), x1, y1) in
				push ((), xy0, xy1)
			)) node))
		in
		{man; eva; mem; map = rec_node}

	let newman man extra = makeman man extra MemoBTable.default_size

	let map man = man.map

	let dump_stats man = MemoBTable.dump_stats man.mem
end

module EBINOP_CACHED(M0:EBINOP_MODELE) =
struct
	module M = M0

	type manager = {
		man : M.M.M.M.manager;
		eva : M.M.manager;
		mem : (M.M.M.M.G.node', Bitv.t, M.M.M.M.G.edge', Bitv.t) Hashcache.tt;
		map : M.M.M.M.G.node' -> M.M.M.M.G.edge';
	}

	let makeman man eva hsize =
		let mem, apply = M.M.M.M.G.(Hashcache.tt_make o3b_node' o3b_edge') hsize in
		let compose = M.M.M.M.M.compose
		and push = M.M.M.M.push man
		and pull = M.M.M.M.pull man
		and eval = M.M.rec_node eva in
		let rec eva_edge (edge, next) = match next with
			| Utils.Leaf leaf -> (false, (edge, Utils.Leaf leaf))
			| Utils.Node(None, node) -> (false, (edge, Utils.Node node))
			| Utils.Node(Some peval, node) -> (true, compose edge (eval peval node))
		and     eva_edge' edge = eva_edge edge |> snd
		and     eva_node' (node, edge0, edge1) =
			(node, eva_edge' edge0, eva_edge' edge1)
		and     eva_node (node, edge0, edge1) =
			let t0, edge0 = eva_edge edge0
			and t1, edge1 = eva_edge edge1 in
			(t0||t1, (node, edge0, edge1))
		and     rec_node node =
			let edge, merge = M.solver(Utils.pnode_of_node node) in
			match merge with
			| Utils.M3Edge next -> eva_edge' (edge, next)
			| Utils.M3Cons node -> compose edge (push(eva_node' node))
			| Utils.M3Node node ->
			let t, node = eva_node node in
			compose edge (if t then (rec_node node) else 
			(apply (fun ((), x, y) -> (
				let ((), x0, x1) = pull x
				and ((), y0, y1) = pull y in
				let xy0 = rec_node ((), x0, y0)
				and xy1 = rec_node ((), x1, y1) in
				push ((), xy0, xy1)
			)) node))
		in
		{man; eva; mem; map = rec_node}

	let newman man extra = makeman man extra Hashcache.default_size

	let map man = man.map

	let dump_stats man = Hashcache.tt_dump_stats man.mem
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

module IMPORT_CACHED(M0:MODULE_SIG) =
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

	include BinUbdag.EXPORT_CACHED(MODELE)

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

module type EXPORT_MODELE =
sig
	module M : MODULE_SIG

	type xedge
	type xedge'
	type extra

	val o3_xedge : (xedge, xedge') O3.o3
	
	val map_edge: extra -> (M.G.edge' -> xedge) -> M.G.edge' -> xedge
	val push:     extra -> unit * xedge * xedge -> xedge

end

module EXPORT(M:EXPORT_MODELE) =
struct

	type manager = {
		man : M.M.manager;
		extra : M.extra;
		mem : (M.M.G.edge', Bitv.t, M.xedge, M.xedge') MemoBTable.t;
		map_edge : M.M.G.edge' -> M.xedge;
	}

	let dump_stats man = MemoBTable.dump_stats man.mem

	let makeman man extra hsize =
		let mem, apply = MemoBTable.make M.M.G.o3b_edge' M.o3_xedge hsize in
		let rec map_edge edge = apply (M.map_edge extra rec_edge) edge
		and     rec_edge edge =
			let ((), edge0, edge1) = M.M.pull man edge in
			M.push extra ((), map_edge edge0, map_edge edge1)
		in
		{man; extra; mem; map_edge}

	let newman man extra = makeman man extra MemoBTable.default_size
	
	let map_edge man = man.map_edge
end

module EXPORT_CACHED(M:EXPORT_MODELE) =
struct

	type manager = {
		man : M.M.manager;
		extra : M.extra;
		mem : (M.M.G.edge', Bitv.t, M.xedge, M.xedge') Hashcache.tt;
		map_edge : M.M.G.edge' -> M.xedge;
	}

	let dump_stats man = Hashcache.tt_dump_stats man.mem

	let makeman man extra hsize =
		let mem, apply = Hashcache.tt_make M.M.G.o3b_edge' M.o3_xedge hsize in
		let rec map_edge edge = apply (M.map_edge extra rec_edge) edge
		and     rec_edge edge =
			let ((), edge0, edge1) = M.M.pull man edge in
			M.push extra ((), map_edge edge0, map_edge edge1)
		in
		{man; extra; mem; map_edge}

	let newman man extra = makeman man extra Hashcache.default_size
	
	let map_edge man = man.map_edge
end

module type EXPORT_NOC_MODELE =
sig
	module M : MODULE_SIG

	type xedge
	type extra

	val map_edge: extra -> (M.G.edge' -> xedge) -> M.G.edge' -> xedge
	val push:     extra -> unit * xedge * xedge -> xedge

end

module EXPORT_NOC(M:EXPORT_NOC_MODELE) =
struct

	type manager = {
		man : M.M.manager;
		extra : M.extra;
		mem : (M.M.G.edge', M.xedge) MemoTable.t;
		map_edge : M.M.G.edge' -> M.xedge;
	}

	let dump_stats man = MemoTable.dump_stats man.mem

	let makeman man extra hsize =
		let mem, apply = MemoTable.make hsize in
		let rec map_edge edge = apply (M.map_edge extra rec_edge) edge
		and     rec_edge edge =
			let ((), edge0, edge1) = M.M.pull man edge in
			M.push extra ((), map_edge edge0, map_edge edge1)
		in
		{man; extra; mem; map_edge}

	let newman man extra = makeman man extra MemoTable.default_size
	
	let map_edge man = man.map_edge
end

module type QUANT_MODELE =
sig
	module M : MODULE_SIG

	type peval
	
	val o3s_peval : peval BinO3.o3s

	type next = peval option * M.G.ident

	type next' = next M.M.M.next'
	type edge' = next M.M.M.edge'
	type node' = next M.M.M.node'

	val eval_edge : peval -> edge' ->  edge'
	val eval_node : peval -> node' -> (edge', node', node') Utils.merge3
	
end

module type QUANT_SIG =
sig
	module M : QUANT_MODELE
	
	type manager
	
	val rec_edge : manager -> M.edge' -> M.M.G.edge'
	val rec_node : manager -> M.peval -> M.M.G.ident -> M.M.G.edge'

	val map_edge : manager -> M.peval -> M.M.G.edge' -> M.M.G.edge'
	val map_node : manager -> M.peval -> M.M.G.node' -> M.M.G.edge'

	val dump_stats : manager -> Tree.stree

end


module QUANT(M0:QUANT_MODELE) =
struct
	module M = M0

	type manager = {
		man : M.M.manager;
		mem : (M.peval * M.M.G.ident, Bitv.t, M.M.G.edge', Bitv.t) MemoBTable.t;
		rec_edge : M.edge' -> M.M.G.edge';
		rec_node : M.peval -> M.M.G.ident -> M.M.G.edge';
		map_edge : M.peval -> M.M.G.edge' -> M.M.G.edge';
		map_node : M.peval -> M.M.G.node' -> M.M.G.edge';
	}

	let dump_stats man = MemoBTable.dump_stats man.mem

	let makeman man (solve_node : M.M.G.node' -> M.M.G.edge') hsize =
		let o3sA = BinO3.closure (M.o3s_peval +* M.M.G.o3s_ident)
		and o3sB = M.M.G.o3b_edge' in
		let mem, apply = MemoBTable.make o3sA o3sB hsize in
		mem.MemoBTable.err <- (fun x opa opaa opb opbb ->
			print_newline();
			print_string "@@ PEVAL - man.mem : ";
			print_int x;
			print_newline();
			print_string "opa  :";
			print_string StrDump.(option ignore opa);
			print_newline();
			print_string "opaa :";
			print_string StrDump.(option bitv_hexa opaa);
			print_newline();
			print_string "opb  :";
			print_string StrDump.(option ignore opb);
			print_newline();
			print_string "opbb :";
			print_string StrDump.(option bitv_hexa opbb);
			print_newline();
			()
		);
		let compose = M.M.M.compose in
		let pull =
			let man = M.M.export man in
			M.M.G.pull man
		in
		let rec rec_edge  (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (edge, Utils.Node ident)
				| Some peval -> compose edge (rec_node peval ident)
		and     rec_node peval ident = apply (fun (peval, ident) ->
			map_node peval (pull ident)
		) (peval, ident)
		and     map_node peval node = 
			match M.eval_node peval (Utils.pnode_of_node node) with
			| Utils.M3Edge edge -> rec_edge edge
			| Utils.M3Cons (node, edge0, edge1) ->
				M.M.push man (node, rec_edge edge0, rec_edge edge1)
			| Utils.M3Node (node, edge0, edge1) ->
				solve_node   (node, rec_edge edge0, rec_edge edge1)
		in
		let map_edge peval edge = rec_edge (M.eval_edge peval (Utils.pedge_of_edge edge)) in
		{man; mem; rec_edge; rec_node; map_edge; map_node}

    let newman man solver = makeman man solver MemoBTable.default_size

	let rec_edge man = man.rec_edge
	let rec_node man = man.rec_node
	let map_edge man = man.map_edge
	let map_node man = man.map_node

end

module QUANT_CACHED(M0:QUANT_MODELE) =
struct
	module M = M0

	type manager = {
		man : M.M.manager;
		mem : (M.peval * M.M.G.ident, Bitv.t, M.M.G.edge', Bitv.t) Hashcache.tt;
		rec_edge : M.edge' -> M.M.G.edge';
		rec_node : M.peval -> M.M.G.ident -> M.M.G.edge';
		map_edge : M.peval -> M.M.G.edge' -> M.M.G.edge';
		map_node : M.peval -> M.M.G.node' -> M.M.G.edge';
	}

	let dump_stats man = Hashcache.tt_dump_stats man.mem

	let makeman man (solve_node : M.M.G.node' -> M.M.G.edge') hsize =
		let o3sA = BinO3.closure (M.o3s_peval +* M.M.G.o3s_ident)
		and o3sB = M.M.G.o3b_edge' in
		let mem, apply = Hashcache.tt_make o3sA o3sB hsize in
		let compose = M.M.M.compose in
		let pull =
			let man = M.M.export man in
			M.M.G.pull man
		in
		let rec rec_edge  (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node (opeval, ident) -> match opeval with
				| None -> (edge, Utils.Node ident)
				| Some peval -> compose edge (rec_node peval ident)
		and     rec_node peval ident = apply (fun (peval, ident) ->
			map_node peval (pull ident)
		) (peval, ident)
		and     map_node peval node = 
			match M.eval_node peval (Utils.pnode_of_node node) with
			| Utils.M3Edge edge -> rec_edge edge
			| Utils.M3Cons (node, edge0, edge1) ->
				M.M.push man (node, rec_edge edge0, rec_edge edge1)
			| Utils.M3Node (node, edge0, edge1) ->
				solve_node   (node, rec_edge edge0, rec_edge edge1)
		in
		let map_edge peval edge = rec_edge (M.eval_edge peval (Utils.pedge_of_edge edge)) in
		{man; mem; rec_edge; rec_node; map_edge; map_node}

    let newman man solver = makeman man solver Hashcache.default_size

	let rec_edge man = man.rec_edge
	let rec_node man = man.rec_node
	let map_edge man = man.map_edge
	let map_node man = man.map_node

end
