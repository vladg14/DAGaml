(* UDAG : Unique - Directed Acyclic Diagramm
 *
 *)


module type GRAPH_HEADER = sig
    type leaf
    type edge
    type node
end
module type GRAPH = sig
    include GRAPH_HEADER
    type ident

	type next_t = (leaf, ident) Utils.gnode
	type edge_t = edge * next_t
	type node_t = node * (edge_t list)
    
    type manager
    
    val newman : unit -> manager
    val push : manager -> node_t -> ident
    val pull : manager -> ident -> node_t
	
end
module type UDAG_HEADER = sig
	include GRAPH_HEADER

	val dump_leaf : (leaf -> Tree.stree) option
	val load_leaf : (Tree.stree -> leaf) option
	val dot_of_leaf : (leaf -> string) option

	val dump_edge : (edge -> Tree.stree) option
	val load_edge : (Tree.stree -> edge) option
	val dot_of_edge : (edge -> string) option
	
	val dump_node : (node -> Tree.stree) option
	val load_node : (Tree.stree -> node) option
	val dot_of_node : (int -> node -> string) option

end

module UDAG (Header:UDAG_HEADER) =
struct
	type ident = int

    type leaf = Header.leaf
    type edge = Header.edge
    type node = Header.node

	type next_t = (leaf, ident) Utils.gnode
	type edge_t = edge * next_t
	type node_t = node * (edge_t list)
	
	type manager = {
        unique : node_t H2Table.t;
	}

    
	let makeman hsize = {
        unique = H2Table.create hsize 0;
    }

    let default_newman_hsize = 10000
	let newman () = makeman default_newman_hsize
	
	let push udag = H2Table.push udag.unique
	let pull udag = H2Table.pull udag.unique
	
	let length udag = H2Table.length udag.unique
	
	let dump =
		let dump_leaf = match Header.dump_leaf with
			| Some dump -> dump
			| None		-> (fun _ -> STD.unit())
		and dump_edge = match Header.dump_edge with
			| Some dump -> dump
			| None		-> (fun _ -> STD.unit())
		and dump_node = match Header.dump_node with
			| Some dump -> dump
			| None		-> (fun _ -> STD.unit())
		in
		let dump_next_t (parcours: ident -> unit) = function
			| Utils.Leaf leaf -> [Tree.Leaf "Leaf"; dump_leaf leaf]
			| Utils.Node ident -> parcours ident; [Tree.Leaf "Node"; STD.int ident]
		in
		let dump_edge_t parcours =
            let dump_next_t = dump_next_t parcours in
            function ((edge, next) : edge_t) ->
                Tree.Node ((dump_edge edge)::(dump_next_t next))
		in
		let dump_node_t parcours =
			let dump_edge_t = dump_edge_t parcours in
			function ((node, edges) : node_t) -> (dump_node node)::(List.map dump_edge_t edges)
		in fun udag edges ->
			let memo = MemoTable.create (H2Table.length udag.unique) in
			let apply : (ident -> unit) -> ident -> unit = MemoTable.apply memo in
			let liste = ref [] in
			let push x =
				liste:=(x::(!liste)) in
			let revret () =
				let temp = List.rev (!liste) in
				liste := [];
				temp
			in

			let rec parcours ident : unit = apply	((fun ident ->
				(
					let node : node_t = H2Table.pull udag.unique ident in
					push (Tree.Node ((STD.int ident)::(dump_node_t (parcours: ident -> unit) node)));
				)
											):ident -> unit) (ident:ident)
			in
			let liste_edges =	(
				List.map (dump_edge_t parcours) edges
								)
			in
			let liste_nodes =	revret() in
			Tree.Node [Tree.Node liste_nodes; Tree.Node liste_edges]
	
	let to_dot =
		let dump_leaf = match Header.dot_of_leaf with
			| Some dump -> dump
			| None		-> (fun _ -> "")
		and dump_edge = match Header.dot_of_edge with
			| Some dump -> dump
			| None		-> (fun _ -> "")
		and dump_node = match Header.dot_of_node with
			| Some dump -> dump
			| None		-> (fun _  _ -> "")
		in
		let dump_next_t (parcours: ident -> unit) getleaf = function
			| Utils.Leaf leaf -> "L"^(string_of_int (getleaf leaf))
			| Utils.Node ident -> parcours ident; "N"^(string_of_int ident)
		in
		let dump_edge_t parcours getleaf =
            let dump_next_t = dump_next_t parcours getleaf in
            fun (edge, next) -> (dump_edge edge, dump_next_t next)
		in
		let dump_node_t parcours getleaf =
			let dump_edge_t = dump_edge_t parcours getleaf in
			fun ident (node, edges) -> (dump_node ident node, List.map dump_edge_t edges)
		in fun udag edges print ->
			let memo = MemoTable.create (H2Table.length udag.unique) in
			let memo_leaf = H2Table.create 1 0 in
			let getleaf = H2Table.push memo_leaf in
			let apply : (ident -> unit) -> ident -> unit = MemoTable.apply memo in
			let rec parcours ident : unit = apply	((fun ident ->
				(
					let node : node_t = H2Table.pull udag.unique ident in
					let node_ , edges_ = dump_node_t parcours getleaf ident node in
					let src_ = "N"^(string_of_int ident) in
					print ( "\t"^src_ ^ " " ^ node_ ^ "\n");
					List.iter (fun (edge_, dst_) -> print("\t" ^ src_ ^ " -> " ^ dst_ ^ " " ^ edge_^"\n" )) edges_ ;
				)
											):ident -> unit) (ident:ident)
			in
			List.iteri (fun idx edge ->
				let edge_, dst_ = dump_edge_t parcours getleaf edge in
				print ("\tE"^(string_of_int idx)^" -> "^dst_^" "^edge_^"\n" )) edges
	
	let to_dot_file udag edges target =
		let file = open_out target in
		let print = output_string file in
		print "digraph G {\n\tedge[fontname=\"courier\"];\n";
		to_dot udag edges print;
		print "}\n";
		close_out file

	let load =
		match Header.load_leaf with
			| None -> None
			| Some load_leaf ->
		match Header.load_edge with
			| None -> None
			| Some load_edge ->
		match Header.load_node with
			| None -> None
			| Some load_node ->
		let load_next getid = function
			| [Tree.Leaf "Leaf"; leaf] -> Utils.Leaf (load_leaf leaf)
			| [Tree.Leaf "Node"; ident] ->
					Utils.Node (getid (STL.int ident))
			| _ -> assert false
		in
		let load_edge getid = function
			| Tree.Node (edge::noderef) -> (load_edge edge, load_next getid noderef)
			| _ -> assert false
		in
		let load_node getid = function
			| node::edges -> (load_node node, (List.map (load_edge getid) edges))
			| _ -> assert false
		in Some (function
			| Tree.Node [Tree.Node liste_nodes; Tree.Node liste_edges] ->
			(
				let n = List.length liste_nodes in
				let htbl = Hashtbl.create n in
				let getid = Hashtbl.find htbl
				and add = Hashtbl.add htbl
				and mem = Hashtbl.mem htbl in
				let udag = makeman n in
				let load_node = function
					| Tree.Node (ident::node) ->
						let ident = STL.int ident in
						assert(not(mem ident));
						add ident ((load_node getid node)|>(push udag))
					| _ -> assert false
				in
				List.iter load_node liste_nodes;
				let liste_edges = List.map (load_edge getid) liste_edges in
				udag, liste_edges
			)
			| _ -> assert false
                )
	
	module type MODELE_VISITOR =
	sig
		type xedge
		type xnode
		type extra

		val do_leaf : extra -> leaf -> xnode
		val do_edge : extra -> edge -> xnode -> xedge
		val do_node : extra -> node -> xedge list -> xnode
	end

	module VISITOR(M0:MODELE_VISITOR) =
	struct
		type mymanager = {
			man : manager;
			extra : M0.extra;
			calc : edge_t -> M0.xedge;
			memLeaf : (leaf, M0.xnode) MemoTable.t;
			memEdge : ((edge * M0.xnode), M0.xedge) MemoTable.t;
			memNode : (ident, M0.xnode) MemoTable.t;
		}

		type manager = mymanager

		let makeman man extra hsize =
			let memLeaf, memEdge, memNode = MemoTable.(create hsize, create hsize, create hsize) in
			let appLeaf, appEdge, appNode = MemoTable.(apply memLeaf, apply memEdge, apply memNode) in
			let rec calcrec (edge, next) =
				 calcedge edge (match next with
					| Utils.Leaf leaf -> calcleaf leaf
					| Utils.Node node -> calcnode node)
			and		calcleaf leaf = appLeaf (M0.do_leaf extra) leaf
			and		calcedge edge xnode = appEdge (fun (edge, xnode) -> M0.do_edge extra edge xnode) (edge, xnode)
			and		calcnode ident = appNode (fun ident ->
				let node, edgelist = pull man ident in
				M0.do_node extra node (List.map calcrec edgelist)) ident
			in
			{
				man = man;
				extra = extra;
				calc = calcrec;
				memLeaf = memLeaf;
				memEdge = memEdge;
				memNode = memNode;
			}, calcrec

		let newman man extra = makeman man extra (H2Table.length man.unique)

		let calc man = man.calc

		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "memo leaf:"; MemoTable.dump_stats man.memLeaf];
			Tree.Node [Tree.Leaf "memo edge:"; MemoTable.dump_stats man.memEdge];
			Tree.Node [Tree.Leaf "memo node:"; MemoTable.dump_stats man.memNode];
		]



	end

end


module STRTREE_HEADER : UDAG_HEADER with
		type leaf = Tree.stree
	and type edge = Tree.stree
	and type node = Tree.stree
=
struct
	type leaf = Tree.stree
	type edge = Tree.stree
	type node = Tree.stree

	let dump_leaf = Some (fun x -> x)
	let load_leaf = Some (fun x -> x)
	let dot_of_leaf = Some (fun x -> STree.string_of_stree x)

	let dump_edge = Some (fun x -> x)
	let load_edge = Some (fun x -> x)
	let dot_of_edge = Some (fun x -> STree.string_of_stree x)

	let dump_node = Some (fun x -> x)
	let load_node = Some (fun x -> x)
	let dot_of_node = Some (fun _ x -> STree.string_of_stree x)
end


module STRING_HEADER : UDAG_HEADER with
		type leaf = string
	and type edge = string
	and type node = (int option) * string
=
struct
	type leaf = string
	type edge = string
	type node = (int option) * string

	let dump_leaf = Some (fun x -> Tree.Leaf x)
	let load_leaf = Some (function Tree.Leaf x -> x | _ -> assert false)
	let dot_of_leaf = Some (fun x -> x)

	let dump_edge = Some (fun x -> Tree.Leaf x)
	let load_edge = Some (function Tree.Leaf x -> x | _ -> assert false)
	let dot_of_edge = Some (fun x -> x)

	let dump_node = Some (function (None, x) -> Tree.Leaf x | (Some i, x) -> Tree.Node [STD.int i; Tree.Leaf x])
	let load_node = Some (function
		| Tree.Leaf x -> (None, x)
		| Tree.Node [i; Tree.Leaf x] -> (Some(STL.int i), x)
		| _ -> assert false)
	let dot_of_node = Some (fun _ (_, x) -> x)
end



module String =
struct
	include UDAG(STRING_HEADER)
	let load = match load with
		| Some f	-> f
		| None		-> assert false
	
	let loadfile target =
		load (match STree.loadfile target with [] -> assert false | x::_ -> x)
	
	let dumpfile udag edges target =
		STree.dumpfile [dump udag edges] target

end

module STree =
struct
	include UDAG(STRTREE_HEADER)
	let load = match load with
		| Some f	-> f
		| None		-> assert false
	
	let loadfile target =
		load (match STree.loadfile target with [] -> assert false | x::_ -> x)
	
	let dumpfile udag edges target =
		STree.dumpfile [dump udag edges] target
end
