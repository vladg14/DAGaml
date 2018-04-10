module HeaderTest =
struct
	type node = unit
	type edge = unit
	type leaf = unit

	let dump_node = Some STD.unit
	let load_node = Some STL.unit
	let dot_of_node = None

	let dump_edge = Some STD.unit
	let load_edge = Some STL.unit
	let dot_of_edge = None

	let dump_leaf = Some STD.unit
	let load_leaf = Some STL.unit
	let dot_of_leaf = None

end;;

module type D =
sig
	module H : Udag.GRAPH_HEADER
	type node = H.node
	type edge = H.edge
	type leaf = H.leaf
end;;

module D1(H:Udag.GRAPH_HEADER) : D =
struct
	module H = H
	type node = H.node
	type edge = H.edge
	type leaf = H.leaf
end;;

module TestDAG = Udag.UDAG(HeaderTest);;
module ReduceDAG = Reduce.Create(TestDAG)(TestDAG);;
