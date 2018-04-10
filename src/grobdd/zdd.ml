(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

let strdump_node = STD.bitv_hexa
let strload_node = Extra.(STL.bitv_hexa >> ZddGops.load_stream ZddGops.binload_node >> ZddGops.node_split)

let strdump_edge = Extra.(ZddGops.dump_stream ZddGops.bindump_edge >> STD.bitv_hexa)
let strload_edge = Extra.(STL.bitv_hexa >> ZddGops.load_stream ZddGops.binload_edge)

let dot_of_edge_aux color block =
	"[label = \""^(ZddGops.strdump_edge block)^"\"; color=\""^color^"\"];"

let dot_of_edge = dot_of_edge_aux "black"

let dot_of_node node =
	let e0, e1 = ZddGops.load_stream ZddGops.binload_node node |> ZddGops.node_split in
	"", (dot_of_edge_aux "red" e0), (dot_of_edge_aux "blue" e1)

let default_leaf = ((0, 0), Utils.Leaf false)

let strdump_leaf = STD.bool
let strload_leaf = Extra.(STL.bool >> (fun b -> ((0, 0), Utils.Leaf b)))

module GroBdd_M : Subdag.MODELE with
		type node = ZddTypes.node_cstate
	and	type edge = ZddTypes.edge_state
	and type leaf = bool
=
struct
	
	type node = ZddTypes.node_cstate
	type edge = ZddTypes.edge_state
	type leaf = bool

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push : ('t -> 'i) -> 't e -> 't e -> ('t e, edge * 't n) Utils.merge =
		ZddGops.node_push_cons
	let pull = ZddGops.node_pull
	let compose = ZddGops.compose
	
	let pull_node = ZddGops.node_pull_node
	
	let dump_node   = Some strdump_node
	let load_node   = Some strload_node
	let dot_of_node = Some dot_of_node
	
	let dump_edge   = Some strdump_edge
	let load_edge   = Some strload_edge
	let dot_of_edge = Some dot_of_edge

	let dump_leaf   = Some strdump_leaf
	let load_leaf   = Some strload_leaf
	let dot_of_leaf = Some (function true -> "[label = \"1\"];" | false -> "[label = \"0\"];")
end

module GroBdd =
struct
	include Subdag.MODULE(GroBdd_M)
	let dumpfile man edges target =
		let strman = Udag.STree.newman() in
		let stredges = dump man strman edges in
		Udag.STree.dumpfile strman stredges target
	
	let loadfile target =
		let strman, stredges = Udag.STree.loadfile target in
		let man = newman () in
		let edges = load man strman stredges in
		man, edges

	let stree_dump man edges =
		let strman = Udag.STree.newman() in
		let stredges = dump man strman edges in
		Udag.STree.dump strman stredges

	let stree_load stree =
		let strman, stredges = Udag.STree.load stree in
		let man = newman () in
		let edges = load man strman stredges in
		man, edges
end

module GetSize =
struct
	module MODELE_VISITOR =
	struct
		type xnode = unit
		type xedge = unit
		type extra = int ref

		let do_leaf extra (_:GroBdd.M.leaf) = extra := !extra + 1; ()
		let do_node extra (c:GroBdd.M.node) = extra := !extra + Bitv.length c; Utils.MNode (fun () () -> ())
		let do_edge extra _ () = ()
	end

	module VISITOR = GroBdd.NODE_VISITOR(MODELE_VISITOR)

	let newman man =
		VISITOR.newman man (ref 0)
	
	let dump_stats = VISITOR.dump_stats

	let get man = !(VISITOR.extra man)
	
end
