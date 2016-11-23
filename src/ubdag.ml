module type HEADER = sig
	type node
	type leaf
end

module UBDAG(Header:HEADER) =
struct
	type ident = int

	type tree =
		| Leaf of Header.leaf
		| Node of ident * node
	and	 node = (Header.node * tree * tree)
	
	let equal_tree x y = match x, y with
		| Leaf leaf, Leaf leaf' -> leaf = leaf'
		| Node (idx, _), Node (idx', _) -> idx = idx'
		| _ -> false

	let equal_node (d, t0, t1) (d', t0', t1') =
		(d = d')&&(equal_tree t0 t0')&&(equal_tree t1 t1')

	module HTree : Hashtbl.HashedType with type t = node =
	struct
		type t = node
		let equal = equal_node

		let hash_tree = function
			| Leaf leaf		-> Hashtbl.hash leaf
			| Node (h, _)	-> h
		let hash (d, x, y) = Hashtbl.hash (d, hash_tree x, hash_tree y)
	end
	module Unique = Ephemeron.K1.Make(HTree)
	
	type manager = {
		ident : ident ref;
		unique : tree Unique.t;
	}

	let makeman hsize = {
		ident = ref 0;
		unique = Unique.create hsize;
	}

	let default_newman_hsize = 10000

	let newman () = makeman default_newman_hsize
	let push man node =
		try
			Unique.find man.unique node
		with Not_found ->
		(
			let tree = Node (!(man.ident), node) in
			incr (man.ident);
			Unique.add man.unique node tree;
			tree
		)
	
	let pull man = function
		| Leaf _ -> assert false
		| Node (ident, node) -> node
	
end
