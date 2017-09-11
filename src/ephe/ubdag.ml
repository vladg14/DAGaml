module M0T(ODeco:Map.OrderedType) =
struct
	module MemoD = Map.Make(ODeco) 
	type deco = ODeco.t
	type 'a manager = {
		assoc  : 'a MemoD.t ref;
		hitCnt : int ref;
		clcCnt : int ref;
	}

	let makeman hsize = {
		assoc  = ref MemoD.empty;
		hitCnt = ref 0;
		clcCnt = ref 0;
	}

	let dump_stats man = Tree.Leaf ("Hit: "^(string_of_int (!(man.hitCnt)))^"; Clc: "^(string_of_int (!(man.clcCnt))))

	let newman_default_hsize = 10000
	
	let newman () = makeman newman_default_hsize

	let test man d =
		MemoD.mem d (!(man.assoc))
	
	let push man d x =
		man.assoc := (MemoD.add d x (!(man.assoc)));
		()
	
	let pull man d =
		try
			Some(MemoD.find d (!(man.assoc)))
		with
			Not_found -> None

	let apply (man:'a manager) (func:(deco -> 'a)) (d:deco) =
		try
		(
			let x = MemoD.find d (!(man.assoc)) in
			incr man.hitCnt;
			x
		)
		with	Not_found ->
		(
			let x = func d in
			incr man.clcCnt;
			assert(not (MemoD.mem d (!(man.assoc))));
			man.assoc := (MemoD.add d x (!(man.assoc)));
			x
		)
end


module type MODULE_UBDAG =
sig
	module H:Udag.UDAG_HEADER

	type ident = int
(*	type ident *)

	type tree  = (H.leaf, pnode) Utils.gnode
	and  node  = H.node * tree * tree
	and  pnode = {ident : ident; node : node}
(*	and	 pnode *)

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

		val dump_stats : 'value manager -> Tree.stree
	
		val test : 'value manager -> bool nnd
		val push : 'value manager -> ('value -> unit) nnd
		
		val pull : 'value manager -> ('value option) nnd

		val apply : 'value manager -> ('value nnd) -> 'value nnd
	end
	
	module M1T(ODeco:Map.OrderedType):
	sig
		type deco = ODeco.t
		type 'a nd = pnode -> deco -> 'a
		type 'value manager

		val makeman : int -> 'value manager
		val newman  : unit -> 'value manager
		
		val dump_stats : 'value manager -> Tree.stree
	
		val test : 'value manager -> bool nd
		val push : 'value manager -> ('value -> unit) nd
		
		val pull : 'value manager -> ('value option) nd

		val apply : 'value manager -> ('value nd) -> 'value nd
	end

	module MLeaf:
	sig
		type 'value manager

		val makeman : int -> 'value manager
		val newman : unit -> 'value manager

		val dump_stats : 'value manager -> Tree.stree
		val apply : 'value manager -> (H.leaf -> 'value) -> H.leaf -> 'value
	end

	module MNode:
	sig
		type 'value manager

		val makeman : int -> 'value manager
		val newman  : unit -> 'value manager

		val dump_stats : 'value manager -> Tree.stree
		val apply : 'value manager -> (pnode -> 'value) -> pnode -> 'value
	end

end


module UBDAG(H0:Udag.UDAG_HEADER) : MODULE_UBDAG with
		type H.node = H0.node
	and type H.leaf = H0.leaf
=
struct
	type ident = int

	module H = H0
	type tree = (H.leaf, pnode) Utils.gnode
	and	 node  = (H.node * tree * tree)
	and  pnode = {ident : ident; node : node}

	let get_ident pnode = pnode.ident
	
	let equal_tree x y = match x, y with
		| Utils.(Leaf x, Leaf y) -> x = y
		| Utils.(Node x, Node y) -> (x.ident = y.ident)&&(assert(x.node==y.node); true)
		| _ -> false

	let equal_node (d, t0, t1) (d', t0', t1') =
		(d = d')&&(equal_tree t0 t0')&&(equal_tree t1 t1')

	(* let dump man ed *)

	module HNode : Hashtbl.HashedType with type t = node =
	struct
		type t = node
		let equal = equal_node

		let hash_tree = function
			| Utils.Leaf leaf	-> Hashtbl.hash leaf
			| Utils.Node node	-> node.ident
		let hash (d, x, y) = Hashtbl.hash (d, hash_tree x, hash_tree y)
	end
	module HPNode : Hashtbl.HashedType with type t = pnode =
	struct
		type t = pnode
		let equal = (=)

		let hash pnode = pnode.ident
	end
	module Unique : Hashtbl.S with type key = node = Ephemeron.K1.Make(HNode)
	
	type manager = {
		index  : ident ref;
		unique : pnode Unique.t;
	}

	let makeman hsize = {
		index  = ref 0;
		unique = Unique.create hsize;
	}

	let default_newman_hsize = 10000

	let newman () = makeman default_newman_hsize
	let push man node =
		try
			Unique.find man.unique node
		with Not_found ->
		(
			let pnode = {ident = !(man.index); node = node} in
			incr (man.index);
			Unique.add man.unique node pnode;
			pnode
		)
	
	let pull _ pnode = pnode.node
	
	module M2T(ODeco:Map.OrderedType) =
	struct
		module MemoNN = Ephemeron.K2.Make(HPNode)(HPNode)
		module MemoD = Map.Make(ODeco) 
		type deco = ODeco.t
		type 'a nnd = HPNode.t -> HPNode.t -> MemoD.key -> 'a
		type 'a manager = {
			assoc  : (('a MemoD.t) ref MemoNN.t);
			hitCnt : int ref;
			clcCnt : int ref;
		}

		let makeman hsize = {
			assoc  = MemoNN.create hsize;
			hitCnt = ref 0;
			clcCnt = ref 0;
		}

		let dump_stats man = Tree.Leaf ("Hit: "^(string_of_int (!(man.hitCnt)))^"; Clc: "^(string_of_int (!(man.clcCnt))))

		let newman_default_hsize = 10000
		
		let newman () = makeman newman_default_hsize

		let test man n0 n1 d =
			try
				MemoD.mem d (!(MemoNN.find man.assoc (n0, n1)))
			with
				Not_found -> false
		
		let push man n0 n1 d x =
			try
				let memoD = MemoNN.find man.assoc (n0, n1) in
				memoD := (MemoD.add d x (!memoD));
			with
				Not_found ->
					MemoNN.add man.assoc (n0, n1) (ref(MemoD.(empty |> add d x)));
			()
		
		let pull man n0 n1 d =
			try 
				let memoD = !(MemoNN.find man.assoc (n0, n1)) in
				Some(MemoD.find d memoD)
			with
				Not_found -> None

		let apply man func n0 n1 =
			let func = func n0 n1 in
			let aux memoD d =
				try
				(
					let x = MemoD.find d (!memoD) in
					incr man.hitCnt;
					x
				)
				with	Not_found ->
				(
					let x = func d in
					incr man.clcCnt;
					assert(not (MemoD.mem d (!memoD)));
					memoD := (MemoD.add d x (!memoD));
					x
				)
			in
			try
				aux (MemoNN.find man.assoc (n0, n1))
			with
				Not_found ->
				let memoD = ref (MemoD.empty) in
				MemoNN.add man.assoc (n0, n1) memoD;
				aux memoD	
	end
	
	module M1T(ODeco:Map.OrderedType) =
	struct
		module MemoN = Ephemeron.K1.Make(HPNode)
		module MemoD = Map.Make(ODeco) 
		type deco = ODeco.t
		type 'a nd = HPNode.t -> deco -> 'a
		type 'a manager = {
			assoc  : (('a MemoD.t) ref MemoN.t);
			hitCnt : int ref;
			clcCnt : int ref;
		}

		let dump_stats man = Tree.Leaf ("Hit: "^(string_of_int (!(man.hitCnt)))^"; Clc: "^(string_of_int (!(man.clcCnt))))

		let makeman hsize = {
			assoc  = MemoN.create hsize;
			hitCnt = ref 0;
			clcCnt = ref 0;
		}

		let dump_stats man = Tree.Leaf ("Hit: "^(string_of_int (!(man.hitCnt)))^"; Clc: "^(string_of_int (!(man.clcCnt))))

		let newman_default_hsize = 10000
		
		let newman () = makeman newman_default_hsize

		let test man n d =
			try
				MemoD.mem d (!(MemoN.find man.assoc n))
			with
				Not_found -> false
		
		let push man n d x =
			try
				let memoD = MemoN.find man.assoc n in
				memoD := (MemoD.add d x (!memoD));
			with
				Not_found ->
					MemoN.add man.assoc n (ref(MemoD.(empty |> add d x)))
		
		let pull man n d =
			try 
				let memoD = !(MemoN.find man.assoc n) in
				Some(MemoD.find d memoD)
			with
				Not_found -> None

		let apply man func n =
			let func = func n in
			let aux memoD d =
				try
				(
					let x = MemoD.find d (!memoD) in
					incr man.hitCnt;
					x
				)
				with	Not_found ->
				(
					let x = func d in
					incr man.clcCnt;
					assert(not (MemoD.mem d (!memoD)));
					memoD := (MemoD.add d x (!memoD));
					x
				)
			in
			try
				aux (MemoN.find man.assoc n)
			with
				Not_found ->
				let memoD = ref (MemoD.empty) in
				MemoN.add man.assoc n memoD;
				aux memoD	
	end
	module MLeaf =
	struct
		module H =
		struct
			type t = H.leaf
			let compare = Pervasives.compare
		end
		module M = M0T(H)

		type 'a manager = 'a M.manager
		let makeman = M.makeman
		let newman = M.newman

		let dump_stats = M.dump_stats
		let apply = M.apply
	end
	module MNode =
	struct
		module ET = Ephemeron.K1.Make(HPNode)
		type 'a manager = {
			assoc  : 'a ET.t;
			hitCnt : int ref;
			clcCnt : int ref;
		}

		let dump_stats man = Tree.Leaf ("Hit: "^(string_of_int (!(man.hitCnt)))^"; Clc: "^(string_of_int (!(man.clcCnt))))

		let makeman hsize = {
			assoc  = ET.create hsize;
			hitCnt = ref 0;
			clcCnt = ref 0;
		}

		let dump_stats man = Tree.Leaf ("Hit: "^(string_of_int (!(man.hitCnt)))^"; Clc: "^(string_of_int (!(man.clcCnt))))

		let newman_default_hsize = 10000
		
		let newman () = makeman newman_default_hsize

		let apply man func n =
			try
				let x = ET.find man.assoc n in
				incr man.hitCnt;
				x
			with
				Not_found ->
					let x = func n in
					incr man.clcCnt;
					ET.add man.assoc n x;
					x
	end

end

