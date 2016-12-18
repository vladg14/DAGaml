(* URDAG : Unique Recursive - Directed Acyclic Diagramm
 *)
module type RGRAPH = sig
	include Udag.GRAPH_HEADER
	type ident

	type next_t =
		| Output	of int
		| Leaf		of leaf
		| NodeRef	of ident
	type edge_t = edge * next_t
	type node_r =
		| Node		of node
		| Graph		of edge_t
	type node_t = node_r * (edge_t list)
	
	type manager
	
	val newman : unit -> manager
	val push : manager -> node_t -> ident
	val pull : manager -> ident -> node_t
end

module URDAG	(Header:Udag.UDAG_HEADER) =
struct
	type ident = int
    
	type leaf = Header.leaf
    type edge = Header.edge
    type node = Header.node

	type next_t =
		| Output	of int
		| Leaf		of leaf
		| NodeRef	of ident
	type edge_t = edge * next_t
	type node_r =
		| Node		of node
		| Graph		of edge_t
	type node_t = node_r * (edge_t list)

	type manager = {
		unique : node_t H2Table.t
	}

	let default_newman_hsize = 10000
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
			| None		-> (fun _ -> StrTree.of_unit())
		and dump_edge = match Header.dump_edge with
			| Some dump -> dump
			| None		-> (fun _ -> StrTree.of_unit())
		and dump_node = match Header.dump_node with
			| Some dump -> dump
			| None		-> (fun _ -> StrTree.of_unit())
		in
		let dump_next_t parcours = function
			| Output idx -> [Tree.Leaf "Output"; StrTree.of_int idx]
			| Leaf leaf -> [Tree.Leaf "Leaf"; dump_leaf leaf]
			| NodeRef ident -> [Tree.Leaf "NodeRef"; StrTree.of_int ident]
		in
		let dump_edge_t parcours =
            let dump_next_t = dump_next_t parcours in
            function ((edge, next) : edge_t) ->
                Tree.Node ((dump_edge edge)::(dump_next_t next))
		in
		let dump_node_r parcours =
			let dump_edge_t = dump_edge_t parcours in
			function
				| Node	node -> Tree.Node [Tree.Leaf "Node"; dump_node node]
				| Graph edge -> Tree.Node [Tree.Leaf "Graph"; dump_edge_t edge]
		in
		let dump_node_t parcours =
			let dump_edge_t = dump_edge_t parcours in
			let dump_node_r = dump_node_r parcours in
			function ((node, edges) : node_t) -> (dump_node_r node)::(List.map dump_edge_t edges)
		in fun udag edges ->
			let memo = MemoTable.create (H2Table.length udag.unique) in
			let apply = MemoTable.apply memo in
			let liste = ref [] in
			let push x = liste:=(x::(!liste)) in
			let revret () =
				let temp = List.rev (!liste) in
				liste := [];
				temp
			in

			let rec parcours ident = apply ident	(fun ident ->
				(
					let node : node_t = H2Table.pull udag.unique ident in
					push (Tree.Node ((StrTree.of_int ident)::(dump_node_t parcours node)))
				)
													)
			in
			let liste_edges =	(
				List.map (dump_edge_t parcours) edges
								)
			in
			let liste_nodes =	revret() in
			Tree.Node [Tree.Node liste_nodes; Tree.Node liste_edges]
			
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
		let load_next_t getid = function
			| [Tree.Leaf text; b] ->
			(
				match text with
				| "Output"	-> Output (StrTree.to_int b)
				| "Leaf"	-> Leaf (load_leaf b)
				| "NodeRef"	-> NodeRef (getid(StrTree.to_int b))
				| _ -> assert false
			)
			| _ -> assert false
		in
		let load_edge_t getid = function
			| Tree.Node (edge::noderef) -> (load_edge edge, load_next_t getid noderef)
			| _ -> assert false
		in
		let load_node_r getid = function
			| Tree.Node [Tree.Leaf tag; b] ->
			(
				match tag with
				| "Node" -> Node (load_node b)
				| "Graph" -> Graph (load_edge_t getid b)
				| _ -> assert false
			)
			| _ -> assert false
		in
		let load_node_t getid = function
			| node::edges -> (load_node_r getid node, (List.map (load_edge_t getid) edges))
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
						let ident = StrTree.to_int ident in
						assert(not(mem ident));
						add ident ((load_node_t getid node)|>(push udag))
					| _ -> assert false
				in
				List.iter load_node liste_nodes;
				let liste_edges = List.map (load_edge_t getid) liste_edges in
				udag, liste_edges
			)
			| _ -> assert false
                )
	
end
