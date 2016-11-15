open Udag

module HeaderTest =
struct
	type node = unit
	type edge = unit
	type leaf = unit

	let dump_node = Some StrTree.of_unit
	let load_node = Some StrTree.to_unit
	let dot_of_node = None

	let dump_edge = Some StrTree.of_unit
	let load_edge = Some StrTree.to_unit
	let dot_of_edge = None

	let dump_leaf = Some StrTree.of_unit
	let load_leaf = Some StrTree.to_unit
	let dot_of_leaf = None

end;;

module TestDAG = UDAG(HeaderTest);;
module ReduceDAG = Reduce.Create(TestDAG)(TestDAG);;
