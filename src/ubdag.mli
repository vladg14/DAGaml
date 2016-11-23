module type HEADER = sig
	type node
	type leaf
end

module UBDAG(Header:HEADER):
sig
	type ident

	type tree  =
		| Leaf of Header.leaf
		| Node of pnode
	and  node  = Header.node * tree * tree
	and	 pnode

	val get_ident : pnode -> ident
	
	val equal_tree : tree -> tree -> bool
	val equal_node : node -> node -> bool

	type manager

	val makeman : int -> manager

	val newman : unit -> manager
	val push : manager -> node -> pnode
	val pull : manager -> pnode -> node
	
	module M2T(ODeco:Map.OrderedType):
	sig
		type deco = ODeco.t
		type 'a nnd = pnode -> pnode -> deco -> 'a
		type 'value manager

		val makeman : int -> 'value manager
		val newman  : unit -> 'value manager
	
		val test : 'value manager -> bool nnd
		val push : 'value manager -> ('value -> unit) nnd
		
		val pull : 'value manager -> ('value option) nnd

		val apply : 'value manager -> ('value nnd) -> 'value nnd
	end
	
	module M1T(ODeco:Map.OrderedType) :
	sig
		type deco = ODeco.t
		type 'a nd = pnode -> deco -> 'a
		type 'value manager

		val makeman : int -> 'value manager
		val newman  : unit -> 'value manager
	
		val test : 'value manager -> bool nd
		val push : 'value manager -> ('value -> unit) nd
		
		val pull : 'value manager -> ('value option) nd

		val apply : 'value manager -> ('value nd) -> 'value nd
	end
end
