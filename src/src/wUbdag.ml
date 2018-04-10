open Extra
open O3Extra

type ('leaf, 'link) next' = ('leaf, 'link) Utils.gnode
type ('edge, 'leaf, 'link) edge' = 'edge * ('leaf, 'link) next'
type ('node, 'edge, 'leaf, 'link) node' = 'node * ('edge, 'leaf, 'link) edge' * ('edge, 'leaf, 'link) edge'

module type MODELE =
sig
	type leaf
	type edge
	type node

	val strdump_leaf : leaf -> string
	val strdump_edge : edge -> string
	val strdump_node : node -> string

	val o3s_leaf : leaf BinUtils.o3s
	val o3s_edge : edge BinUtils.o3s
	val o3s_node : node BinUtils.o3s

	type 'i next' = (leaf, 'i) Utils.gnode
	type 'i edge' = edge * 'i next'
	type 'i node' = node * 'i edge' * 'i edge'

	val o3kls_next' :
		'i HBTree.o3kls -> 'i next' HBTree.o3kls
	val o3kls_edge' :
		'i HBTree.o3kls -> 'i edge' HBTree.o3kls
	val o3kls_node' :
		'i HBTree.o3kls -> 'i node' HBTree.o3kls

	val o3b_next' : ('i next', Bitv.t * ('i option)) O3.o3
	val o3b_edge' : ('i edge', Bitv.t * ('i option)) O3.o3
	val o3b_node' : ('i node', HBTree.opnode) O3.o3
	
	(* assert(x |> dump |> load = x) && assert(x |> load |> dump |> stream) *)
	val __check_reverse__ : bool

end

module type MODULE_SIG =
sig
	type ident
	type link'

	module M : MODELE

	type next' = link' M.next'
	type edge' = link' M.edge'
	type node' = link' M.node'

	type manager

	val makeman : int -> manager
	val newman : unit -> manager

	val dump_stats : manager -> Tree.stree

	val push : manager -> node' -> link'
	val pull : manager -> link' -> node'

	val strdump_ident : ident -> string
	val o3s_ident : ident BinUtils.o3s

	val strdump_next' : next' -> string
	val o3kls_next' : next' HBTree.o3kls
	val  o3kl_next' : next' HBTree.o3kl

	val strdump_edge' : edge' -> string
	val o3kls_edge' : edge' HBTree.o3kls
	val  o3kl_edge' : edge' HBTree.o3kl

	val strdump_node' : node' -> string
	val o3kls_node' : node' HBTree.o3kls
	val  o3kl_node' : node' HBTree.o3kl

	val dump : manager -> edge' list -> Tree.stree
	val load : Tree.stree  -> manager * edge' list

end

module MODULE(M0:MODELE) =
struct
	module M = M0
	type ident = int

	type next' = ident M.next'
	type edge' = ident M.edge'
	type node' = ident M.node'

	module HS = HBTree.Weak

	type manager = HS.t

	let makeman hsize = HS.create hsize 0
	let newman () = HS.create HBTree.default_size 0

	let getsize man =
		let funmap link = match HBTree.unlink link with (_, (bitv, _, _)) -> Bitv.length bitv in
		HS.mapreduce man 0 funmap (+)

	let dump_stats man = Tree.(Node [
		Node [Leaf "#node: "; STD.int (HS.length man)];
		Node [Leaf "size:  "; STD.int (getsize man)];
		])

	let strdump_ident = string_of_int
	let o3s_ident = BinO3.int

	let strdump_link' link = link |> HBTree.unlink |> fst |> strdump_ident

	let strdump_next' = function
		| Utils.Leaf leaf -> "Leaf ("^(M.strdump_leaf leaf)^")"
		| Utils.Node link -> "Node ("^(strdump_link'  link)^")"
	let o3kls_next' = M.o3kls_next' HBTree.o3kls_link
	let  o3kl_next' = HBTree.o3closure o3kls_next'

	let strdump_edge' = StrDump.pair M.strdump_edge strdump_next'
	let o3kls_edge' = M.o3kls_edge' HBTree.o3kls_link
	let  o3kl_edge' = HBTree.o3closure o3kls_edge'

	let strdump_node' = StrDump.trio M.strdump_node strdump_edge' strdump_edge'
	let o3kls_node' = M.o3kls_node' HBTree.o3kls_link
	let  o3kl_node' = HBTree.o3closure o3kls_node'

	let push man node =
		let bnode : HBTree.node = node
			|> fst M.o3b_node'
			|> HBTree.node_of_opnode in
		if M.__check_reverse__
		then
		(
			let node' : node' = bnode
				|> HBTree.opnode_of_node 
				|> snd M.o3b_node' in
			assert(node' = node);
		);
		HS.push man bnode

	let pull man link =
		let bnode = HBTree.unlink link |> snd in
		let node = bnode
			|> HBTree.opnode_of_node
			|> snd M.o3b_node' in
		if M.__check_reverse__
		then
		(
			let bnode' = node
				|> fst M.o3b_node'
				|> HBTree.node_of_opnode in
			assert(bnode = bnode')
		);
		node
	
	let dump man edges =
		let rec get_links carry carry' = function
			| [] -> (List.rev carry, List.rev carry')
			| edge::tail ->
				let bitv, opnext = fst M.o3b_edge' edge in
				(* TODO : move to Bitv *)
				let bitv_push (bitv:Bitv.t) b : Bitv.t =
					Bitv.L.(of_bool_list (b::(to_bool_list bitv)))
				in
				let carry, carry' = match opnext with
				| None -> ((bitv_push bitv false)::carry, carry')
				| Some link -> ((bitv_push bitv true)::carry, link::carry')
				in get_links carry carry' tail
		in
		let bedges, links = get_links [] [] edges in
		let man = HS.strdump man links
		and bedges = List.map STD.bitv_hexa bedges in
		Tree.Node (man::bedges)


	let load' hsize = function
		| Tree.Node (man::bedges) ->
		(
			let man, links = HS.strload man 
			and bedges = List.map STL.bitv_hexa bedges in
			(* TODO : move to Bitv *)
			let bitv_pull bitv = Bitv.L.(
				match to_bool_list bitv with
					| [] -> assert false
					| b::bitv -> (b, of_bool_list bitv)
			) in
			let rec get_edges carry = function
				| ([], []) -> List.rev carry
				| (bedge::bedges, links) -> (
					let b, bedge = bitv_pull bedge in
					let oplink, links = if b then (
							let link, links = MyList.hdtl links in
							(Some link, links)
						) else (None, links) in
					let edge = snd M.o3b_edge' (bedge, oplink) in
					get_edges (edge::carry) (bedges, links)
				)
				| ([], _) -> assert false
			in
			let edges = get_edges [] (bedges, links) in
			(man, edges)
		)
		| _ -> assert false

	let load stree = load' HBTree.default_size stree

end

(* WIP *)

let default_o3s_node o3s_core o3s_leaf o3s_link =
	let o3s_next = Utils.o3s_gnode o3s_leaf o3s_link in
	let o3s_meta = O3.trio (o3s_core, o3s_next, o3s_next) in
	let o3_front = O3Utils.from_a_bc_de_to_abd_c_e in
	o3_front +>> o3s_meta

module type OOO_MAP_MODELE =
sig
	module SRC : MODULE_SIG
	module DST : MODULE_SIG

	val map_leaf : SRC.M.leaf -> DST.M.leaf
	val map_edge : SRC.M.edge -> DST.M.edge
	val map_node : SRC.M.node -> DST.M.node
end

module OOO_MAP(M0:OOO_MAP_MODELE) =
(* One-On-One MAPping *)
struct
	type manager = {
		src :  M0.SRC.manager;
		dst :  M0.DST.manager;
		man : (M0.SRC.link', M0.DST.link') MemoTable.t;
		map_next : M0.SRC.next' -> M0.DST.next';
		map_edge : M0.SRC.edge' -> M0.DST.edge';
		map_node : M0.SRC.link' -> M0.DST.link';
	}

	let makeman src dst hsize =
		let man, apply = MemoTable.make hsize in
		let rec map_next = function
			| Utils.Leaf leaf -> Utils.Leaf (M0.map_leaf leaf)
			| Utils.Node ident -> Utils.Node (map_node ident)
		and     map_edge (edge, next) = (M0.map_edge edge, map_next next)
		and     map_node ident = apply (fun ident ->
			let (node, edge'0, edge'1) = M0.SRC.pull src ident in
			M0.DST.push dst (M0.map_node node, map_edge edge'0, map_edge edge'1)
		) ident
		in {src; dst; man; map_next; map_edge; map_node}
    
	let newman src dst = makeman src dst MemoTable.default_size

	let man_next man = man.map_next
	let man_edge man = man.map_edge
	let man_node man = man.map_node
end

module REMAN(M:MODULE_SIG) =
struct

	type manager = {
		src : M.manager;
		dst : M.manager;
		mem : (M.link', M.link') MemoTable.t;
		map_node : M.link' -> M.link';
		map_edge : M.edge' -> M.edge';
	}

	let makeman src dst hsize =
		let mem, apply = MemoTable.make hsize in
		let rec map_node ident = apply (fun ident ->
			M.push dst (rec_node (M.pull src ident))
		) ident
		and     rec_node (node, edge0, edge1) =
			(node, map_edge edge0, map_edge edge1)
		and     map_edge (edge, next) = (edge, map_next next)
		and     map_next = function
			| Utils.Leaf leaf -> Utils.Leaf leaf
			| Utils.Node ident -> Utils.Node (map_node ident)
		in {src; dst; mem; map_node; map_edge}
	
	let newman src dst = makeman src dst MemoTable.default_size

	let map_node man = man.map_node
	let map_edge man = man.map_edge

end

module type SEM_MAP_MODELE =
sig
	module SRC : MODULE_SIG
	module DST : MODULE_SIG

	val map_leaf : SRC.M.leaf -> DST.edge'
	val map_edge : SRC.M.edge -> DST.edge' -> DST.edge'
	val map_node : SRC.M.node -> DST.edge' -> DST.edge' ->
		DST.M.edge * (DST.next', DST.node') Utils.merge
end

module SEM_MAP(M0:SEM_MAP_MODELE) =
struct
	type manager = {
		src :  M0.SRC.manager;
		dst :  M0.DST.manager;
		man : (M0.SRC.link', M0.SRC.link', M0.DST.edge', HBTree.kl) MemoBTable.t;
		map_next : M0.SRC.next' -> M0.DST.edge';
		map_edge : M0.SRC.edge' -> M0.DST.edge';
		map_node : M0.SRC.link' -> M0.DST.edge';
	}

	let makeman src dst hsize =
		let man, apply = MemoBTable.make O3.id M0.DST.o3kl_edge' hsize in
		let rec map_next = function
			| Utils.Leaf leaf -> M0.map_leaf leaf
			| Utils.Node ident -> map_node ident
		and     map_edge (edge, next) = M0.map_edge edge (map_next next)
		and     map_node ident = apply (fun ident ->
			let (node, edge'0, edge'1) = M0.SRC.pull src ident in
			let edge', merge = M0.map_node node (map_edge edge'0) (map_edge edge'1) in
			(edge', match merge with
				| Utils.MEdge next' -> next'
				| Utils.MNode node' -> Utils.Node(M0.DST.push dst node'))
		) ident
		in {src; dst; man; map_next; map_edge; map_node}
    
	let newman src dst = makeman src dst MemoBTable.default_size

	let man_next man = man.map_next
	let man_edge man = man.map_edge
	let man_node man = man.map_node
	
end

module SEM_MAP_CACHED(M0:SEM_MAP_MODELE) =
struct
	type manager = {
		src :  M0.SRC.manager;
		dst :  M0.DST.manager;
		man : (M0.SRC.link', M0.SRC.link', M0.DST.edge', HBTree.kl) Hashcache.tt;
		map_next : M0.SRC.next' -> M0.DST.edge';
		map_edge : M0.SRC.edge' -> M0.DST.edge';
		map_node : M0.SRC.link' -> M0.DST.edge';
	}

	let makeman src dst hsize =
		let man, apply = Hashcache.tt_make O3.id M0.DST.o3kl_edge' hsize in
		let rec map_next = function
			| Utils.Leaf leaf -> M0.map_leaf leaf
			| Utils.Node ident -> map_node ident
		and     map_edge (edge, next) = M0.map_edge edge (map_next next)
		and     map_node ident = apply (fun ident ->
			let (node, edge'0, edge'1) = M0.SRC.pull src ident in
			let edge', merge = M0.map_node node (map_edge edge'0) (map_edge edge'1) in
			(edge', match merge with
				| Utils.MEdge next' -> next'
				| Utils.MNode node' -> Utils.Node(M0.DST.push dst node'))
		) ident
		in {src; dst; man; map_next; map_edge; map_node}
    
	let newman src dst = makeman src dst Hashcache.default_size

	let man_next man = man.map_next
	let man_edge man = man.map_edge
	let man_node man = man.map_node
	
end

module type EXPORT_MODELE =
sig
	module M : MODULE_SIG

	type extra
	type xnode
	type xnode'
	type xedge

	val o3_xnode : (xnode, xnode') O3.o3

	val map_node : extra -> (unit -> xnode) M.M.node' -> xnode
	val map_edge : extra -> (unit -> xnode) M.M.edge' -> xedge

end

module EXPORT(M:EXPORT_MODELE) =
struct

	type next'' = (unit -> M.xnode) M.M.M.next'
	type edge'' = (unit -> M.xnode) M.M.M.edge'
	type node'' = (unit -> M.xnode) M.M.M.node'

	type manager = {
		man : M.M.manager;
		extra : M.extra;
		mem : (M.M.link', M.M.link', M.xnode, M.xnode') MemoBTable.t;
		rec_edge : M.M.edge' -> M.xedge;
		rec_node : M.M.link' -> M.xnode;
	}

	let dump_stats man = MemoBTable.dump_stats man.mem

	let makeman man extra hsize =
		let mem, apply = MemoBTable.make O3.id M.o3_xnode hsize in
		let rec map_next : M.M.next' -> next'' = function
			| Utils.Leaf leaf -> Utils.Leaf leaf
			| Utils.Node link -> Utils.Node (fun () -> rec_node link)
		and     map_edge ((edge, next) : M.M.edge') : edge'' =
			(edge, map_next next)
		and     map_node ((node, edge0, edge1) : M.M.node') : node'' =
			(node, map_edge edge0, map_edge edge1)
		and     rec_node (ident : M.M.link') : M.xnode = apply (fun ident ->
			M.map_node extra (map_node (M.M.pull man ident))
		) ident
		in
		let     rec_edge edge = M.map_edge extra (map_edge edge) in
		{man; extra; mem; rec_node; rec_edge}

	let newman man extra = makeman man extra MemoBTable.default_size
	
	let rec_edge man = man.rec_edge
	let rec_node man = man.rec_node
end

module EXPORT_CACHED(M:EXPORT_MODELE) =
struct

	type next'' = (unit -> M.xnode) M.M.M.next'
	type edge'' = (unit -> M.xnode) M.M.M.edge'
	type node'' = (unit -> M.xnode) M.M.M.node'

	type manager = {
		man : M.M.manager;
		extra : M.extra;
		mem : (M.M.link', M.M.link', M.xnode, M.xnode') Hashcache.tt;
		rec_edge : M.M.edge' -> M.xedge;
		rec_node : M.M.link' -> M.xnode;
	}

	let dump_stats man = Hashcache.tt_dump_stats man.mem

	let makeman man extra hsize =
		let mem, apply = Hashcache.tt_make O3.id M.o3_xnode hsize in
		let rec map_next : M.M.next' -> next'' = function
			| Utils.Leaf leaf -> Utils.Leaf leaf
			| Utils.Node link -> Utils.Node (fun () -> rec_node link)
		and     map_edge ((edge, next) : M.M.edge') : edge'' =
			(edge, map_next next)
		and     map_node ((node, edge0, edge1) : M.M.node') : node'' =
			(node, map_edge edge0, map_edge edge1)
		and     rec_node (ident : M.M.link') : M.xnode = apply (fun ident ->
			M.map_node extra (map_node (M.M.pull man ident))
		) ident
		in
		let     rec_edge edge = M.map_edge extra (map_edge edge) in
		{man; extra; mem; rec_node; rec_edge}

	let newman man extra = makeman man extra Hashcache.default_size
	
	let rec_edge man = man.rec_edge
	let rec_node man = man.rec_node
end

module type EXPORT_NOC_MODELE =
sig
	module M : MODULE_SIG

	type extra
	type xnode
	type xedge

	val map_node : extra -> (unit -> xnode) M.M.node' -> xnode
	val map_edge : extra -> (unit -> xnode) M.M.edge' -> xedge

end

module EXPORT_NOC(M:EXPORT_NOC_MODELE) =
struct

	type next'' = (unit -> M.xnode) M.M.M.next'
	type edge'' = (unit -> M.xnode) M.M.M.edge'
	type node'' = (unit -> M.xnode) M.M.M.node'

	type manager = {
		man : M.M.manager;
		extra : M.extra;
		mem : (M.M.link', M.xnode) MemoTable.t;
		rec_edge : M.M.edge' -> M.xedge;
		rec_node : M.M.link' -> M.xnode;
	}

	let dump_stats man = MemoTable.dump_stats man.mem

	let makeman man extra hsize =
		let mem, apply = MemoTable.make hsize in
		let rec map_next : M.M.next' -> next'' = function
			| Utils.Leaf leaf -> Utils.Leaf leaf
			| Utils.Node link -> Utils.Node (fun () -> rec_node link)
		and     map_edge ((edge, next) : M.M.edge') : edge'' =
			(edge, map_next next)
		and     map_node ((node, edge0, edge1) : M.M.node') : node'' =
			(node, map_edge edge0, map_edge edge1)
		and     rec_node (ident : M.M.link') : M.xnode = apply (fun ident ->
			M.map_node extra (map_node (M.M.pull man ident))
		) ident
		in
		let     rec_edge edge = M.map_edge extra (map_edge edge) in
		{man; extra; mem; rec_node; rec_edge}

	let newman man extra = makeman man extra MemoTable.default_size
	
	let rec_edge man = man.rec_edge
	let rec_node man = man.rec_node
end

module type TO_DOT_MODELE =
sig
	module M : MODULE_SIG

	val string_of_leaf : M.M.leaf -> string
	val string_of_edge : bool option -> M.M.edge -> string
	val string_of_node : M.M.node -> string
end

module TO_DOT(M0:TO_DOT_MODELE) = 
struct
	
	module M = M0
	
	module MODELE =
	struct
		module M = M.M

		type extra  = Udag.String.manager
		type xnode  = Udag.String.ident
		type xedge  = Udag.String.edge_t

		type next' = (unit -> xnode) M.M.next'
		type edge' = (unit -> xnode) M.M.edge'
		type node' = (unit -> xnode) M.M.node'

		let rec_next = function
			| Utils.Leaf leaf -> Utils.Leaf (M0.string_of_leaf leaf)
			| Utils.Node node -> Utils.Node (node())

		let rec_edge pos (edge, next) = (
			M0.string_of_edge pos edge,
			rec_next next
		)
		
		let rec_node (node, edge0, edge1) =
			((None, M0.string_of_node node), [rec_edge (Some false) edge0; rec_edge (Some true) edge1])

		let map_edge man edge = rec_edge None edge
		let map_node man node = Udag.String.push man (rec_node node)
		
	end

	module MODULE = EXPORT_NOC(MODELE)

	let dotfile ubdag edges target =
		let strman = Udag.String.newman () in
		let man = MODULE.newman ubdag strman in
		let map = MODULE.rec_edge man in
		let edges' = List.map map edges in
		Udag.String.to_dot_file strman edges' target;
		()
end
