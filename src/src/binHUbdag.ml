open Extra
open O3Extra

type ('leaf, 'link) next' = ('leaf, 'link) Utils.gnode
type ('edge, 'leaf, 'link) edge' = 'edge * ('leaf, 'link) next'
type ('node, 'edge, 'leaf, 'link) node' = 'node * ('edge, 'leaf, 'link) edge' * ('edge, 'leaf, 'link) edge'

module type MODELE =
sig
	module SUB : BinUbdag.MODELE
	module ME  : BinUbdag.MODELE

	type core (* type of the [core] graph *)

	type leaf = SubLeaf of Sub.leaf |  MeLeaf of ME.leaf

	type 'i subnext' = (SUB.leaf, 'i) next'
	type 'i subedge' = (SUB.edge, Sub.leaf, 'i) edge'
	type 'i  meedge' = ( ME.edge, Sub.leaf, 'i) edge'
	type 'i  menode' =  ME.node * 'i meedge' * 'i meedge'

	val push_leaf : core ->  ME.leaf -> (Sub.edge, Sub.leaf, _) edge'
	val pull_leaf : core -> SUB.leaf -> ( ME.edge,  ME.leaf, _) edge'

	val push_edge : core -> 'i ( ME.edge,     leaf, 'i) edge' -> (Sub.edge, Sub.leaf, 'i) edge'
	val pull_edge : core -> 'i (Sub.edge,     leaf, 'i) edge' -> ('subedge' -> 'i  meedge'
	val push_node : core -> 'i  menode' -> 'i subedge'

	(* dumps the whole [core] submodule *)
	val dump_core   : core -> 'i subnext' list -> Tree.stree
	val load_core   : Tree.stree -> core * ('i subnext' list)
	val import_core : core -> Tree.stree -> 'i subnext' list
	(* exports only the minimal amount of information to reconstruct the [core] submodule *)
	val export_core : core -> 'i subnext' list -> Tree.stree
	val dot_of_core : core -> 'i subnext' list -> Udag.STree * (string list)
end


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

	val o3s_next' : 'i BinUtils.o3s -> 'i next' BinUtils.o3s
	val o3s_edge' : 'i BinUtils.o3s -> 'i edge' BinUtils.o3s
	val o3s_node' : 'i BinUtils.o3s -> 'i node' BinUtils.o3s
	
	(* assert(x |> dump |> load = x) && assert(x |> load |> dump |> stream) *)
	val __check_reverse__ : bool

end

module type MODULE_SIG =
sig
	type ident

	module M : MODELE

	type next' = ident M.next'
	type edge' = ident M.edge'
	type node' = ident M.node'

	type manager

	val makeman : int -> manager
	val newman : unit -> manager

	val dump_stats : manager -> Tree.stree

	val push : manager -> node' -> ident
	val pull : manager -> ident -> node'

	val strdump_ident : ident -> string
	val o3s_ident : ident BinUtils.o3s

	val strdump_next' : next' -> string
	val o3s_next' : next' BinUtils.o3s

	val strdump_edge' : edge' -> string
	val o3s_edge' : edge' BinUtils.o3s
	val o3b_edge' : edge' BinUtils.o3b

	val strdump_node' : node' -> string
	val o3s_node' : node' BinUtils.o3s
	val o3b_node' : node' BinUtils.o3b

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

	type manager = {
		man : Bitv.t H2Table.t;
	}

    let makeman hsize = {
        man = H2Table.create hsize 0;
    }

    let newman () = makeman H2Table.default_size

	let getsize man =
		H2Table.mapreduce man.man 0 (fun _ -> Bitv.length) (+)

    let dump_stats man = Tree.Node [
        Tree.Node [Tree.Leaf "#node: "; STD.int (H2Table.length man.man) ];
		Tree.Node [Tree.Leaf "size:  "; STD.int (getsize man)            ];
    ]

	let strdump_ident = string_of_int
	let o3s_ident = BinO3.int

	let strdump_next' = function
		| Utils.Leaf leaf -> "Leaf ("^(M.strdump_leaf leaf)^")"
		| Utils.Node node -> "Node ("^(strdump_ident  node)^")"
	let o3s_next' = M.o3s_next' o3s_ident

	let strdump_edge' = StrDump.pair M.strdump_edge strdump_next'
	let o3s_edge' = M.o3s_edge' o3s_ident

	let strdump_node' = StrDump.trio M.strdump_node strdump_edge' strdump_edge'
	let o3s_node' = M.o3s_node' o3s_ident

	let o3b_edge' = BinO3.closure o3s_edge'
	let o3b_node' = BinO3.closure o3s_node'

	let push man node =
		let bitv = fst o3b_node' node in
		if M.__check_reverse__
		then
		(
			let node' : node' = snd o3b_node' bitv in
			assert(node = node')
		);
		H2Table.push man.man bitv

	let pull man ident =
		let bitv = H2Table.pull man.man ident in
		let node = snd o3b_node' bitv in
		if M.__check_reverse__
		then
		(
			let bitv' = fst o3b_node' node in
			assert(bitv = bitv')
		);
		node
	
	let dump man edges =
		let man = H2Table.strdump (STD.bitv_hexa) man.man
		and edges = List.map (fst o3b_edge' >> STD.bitv_hexa) edges in
		Tree.Node (man::edges)

	let load' hsize = function
		| Tree.Node (man::edges) ->
		(
			let man = H2Table.strload hsize (STL.bitv_hexa) man
			and edges = List.map (STL.bitv_hexa >> snd o3b_edge') edges in
			({man}, edges)
		)
		| _ -> assert false

	let load stree = load' default_newman_hsize stree

end

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
		man : (M0.SRC.ident, M0.DST.ident) MemoTable.t;
		map_next : M0.SRC.next' -> M0.DST.next';
		map_edge : M0.SRC.edge' -> M0.DST.edge';
		map_node : M0.SRC.ident -> M0.DST.ident;
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
		mem : (M.ident, M.ident) MemoTable.t;
		map_node : M.ident -> M.ident;
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
		man : (M0.SRC.ident, M0.SRC.ident, M0.DST.edge', Bitv.t) MemoBTable.t;
		map_next : M0.SRC.next' -> M0.DST.edge';
		map_edge : M0.SRC.edge' -> M0.DST.edge';
		map_node : M0.SRC.ident -> M0.DST.edge';
	}

	let makeman src dst hsize =
		let man, apply = MemoBTable.make O3.id M0.DST.o3b_edge' hsize in
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
		mem : (M.M.ident, M.M.ident, M.xnode, M.xnode') MemoBTable.t;
		rec_edge : M.M.edge' -> M.xedge;
		rec_node : M.M.ident -> M.xnode;
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
		and     rec_node (ident : M.M.ident) : M.xnode = apply (fun ident ->
			M.map_node extra (map_node (M.M.pull man ident))
		) ident
		in
		let     rec_edge edge = M.map_edge extra (map_edge edge) in
		{man; extra; mem; rec_node; rec_edge}

	let newman man extra = makeman man extra MemoBTable.default_size
	
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
		mem : (M.M.ident, M.xnode) MemoTable.t;
		rec_edge : M.M.edge' -> M.xedge;
		rec_node : M.M.ident -> M.xnode;
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
		and     rec_node (ident : M.M.ident) : M.xnode = apply (fun ident ->
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
