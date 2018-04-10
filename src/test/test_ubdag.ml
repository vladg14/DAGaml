open Ubdag

module HeaderTest =
struct
	type edge = unit
	type node = unit
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

module TestUBDAG = UBDAG(HeaderTest);;
