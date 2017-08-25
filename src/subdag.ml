open Utils

module type MODELE = sig
	type leaf
	type edge
	type node

	type 't gn = (leaf, 't) gnode
	type 't n = node * 't gn * 't gn
	type 't e = edge * 't gn

	val push : ('t -> 'i) -> 't e -> 't e -> ('t e, (edge * 't n)) merge
	val pull : ('t -> 'i) -> 't e -> ('t e, 't n) unmerge

	val pull_node : ('t -> 'i) -> 't n -> 't e * 't e
	val compose : edge -> 't e -> 't e	
	
	val dump_leaf : (leaf -> StrTree.tree) option
	val load_leaf : (StrTree.tree -> 't e) option
	val dot_of_leaf : (leaf -> string) option

	val dump_edge : (edge -> StrTree.tree) option
	val load_edge : (StrTree.tree -> edge) option
	val dot_of_edge : (edge -> string) option
	
	val dump_node : (node -> StrTree.tree) option
	val load_node : (StrTree.tree -> edge * edge) option
	val dot_of_node : (node -> string * string * string) option

end


(*module type MODULE_SUBDAG =
(* M = Modele *)
sig
	module M : MODELE_SUBDAG
	module G : Ubdag.MODULE_UBDAG with
			type H.node = M.node
		and type H.leaf = M.leaf 

	type edge = M.edge * G.tree

	type manager

	val newman : unit -> manager
	val makeman : int -> manager

	val push_leaf : M.edge -> M.leaf -> edge
	val pull_node : manager -> G.pnode -> edge * edge

	val push : manager -> edge -> edge -> edge
	val pull : manager -> edge -> edge *  edge
	val compose : M.edge -> edge -> edge
end
*)


module MODULE(M0:MODELE)
(*:MODULE_SUBDAG with
		type M.node = M0.node
	and type M.edge = M0.edge
	and type M.leaf = M0.leaf *)
=
(* M = Modele *)
struct
	module M = M0
	module MODELE =
	struct
		type leaf = M0.leaf
		type edge = M0.edge
		type node = M0.node

		type 't gn = (leaf, 't) gnode
		type 't n = node * 't gn * 't gn
		type 't e = edge * 't gn

		let push = M0.push
		let pull = M0.pull
		let pull_node = M0.pull_node
		let compose = M0.compose
		
		let dump_leaf = None
		let load_leaf = None
		let dot_of_leaf = None

		let dump_edge = None
		let load_edge = None
		let dot_of_edge = None
		
		let dump_node = None
		let load_node = None
		let dot_of_node = None

	end
	module G = Ubdag.UBDAG(MODELE)
	
	type edge = M.edge * G.tree
	
	type manager = G.manager

	let newman = G.newman
	let makeman = G.makeman

	let push_leaf (e:M.edge) (l:M.leaf) = ((e, ((Leaf l):G.tree)):edge)
	let pull_node man (n:G.pnode) = M.pull_node G.get_ident (G.pull man n)

	let push man x y = match M.push G.get_ident x y with
		| MEdge e -> e
		| MNode (e, (n:G.node)) -> (e, Node (G.push man (n:G.node)))

	let pull man ((_, n)as e) = match M.pull G.get_ident e with
		| MEdge (x, y) -> (x, y)
		| MNode f -> match n with
			| Leaf _ -> assert false
			| Node p -> f(G.pull man p)

	let compose = M.compose
		
	let dump_stats = G.dump_stats

	module type MODELE_NODE_VISITOR =
	sig
		type xnode
		type xedge
		type extra

		val do_leaf : extra -> M.leaf -> xnode 
		val do_node : extra -> M.node -> (xnode, xnode -> xnode -> xnode) merge
		val do_edge : extra -> M.edge -> xnode -> xedge

	end

	module NODE_VISITOR(D0:MODELE_NODE_VISITOR)=
	struct
		type memo = {
			man  : manager;
			extra : D0.extra;
			calc : edge -> D0.xedge;
			memoLeaf: ( M.leaf, D0.xnode) MemoTable.t;
			memoEdge: (   edge, D0.xedge) MemoTable.t;
			memoNode: (G.pnode, D0.xnode) MemoTable.t;
		}

		type manager = memo
		
		let makeman man extra hsize =
			let memoLeaf, applyLeaf = MemoTable.make hsize
			and memoEdge, applyEdge = MemoTable.make hsize
			and memoNode, applyNode = MemoTable.make hsize in
			let rec calcrec = function
				| Leaf leaf -> calcleaf leaf
				| Node node -> calcnode node
			and		calcedge edge = applyEdge (fun (edge, gnode) -> D0.do_edge extra edge (calcrec gnode)) edge
			and		calcleaf leaf = applyLeaf (D0.do_leaf extra) leaf
			and		calcnode node = applyNode (fun node ->
				let (node:M.node), (nx:G.tree), (ny:G.tree) = G.pull man node in
				match D0.do_node extra node with
				| MEdge xnode -> xnode
				| MNode merger -> merger (calcrec nx) (calcrec ny)) node
			in
			{
				man  = man;
				extra = extra;
				calc = calcedge;
				memoLeaf = memoLeaf;
				memoEdge = memoEdge;
				memoNode = memoNode;
			}, calcedge
		let newman man extra = makeman man extra 10000
		let calc man = man.calc
		let extra man = man.extra
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "memo leaf:"; MemoTable.dump_stats man.memoLeaf];
			Tree.Node [Tree.Leaf "memo edge:"; MemoTable.dump_stats man.memoEdge];
			Tree.Node [Tree.Leaf "memo node:"; MemoTable.dump_stats man.memoNode]
		]
	end

	module type MODELE_EDGE_VISITOR =
	sig
		type xedge
		type extra

		val do_leaf: extra -> M.edge -> M.leaf -> xedge
		val do_node: extra -> M.edge -> (xedge, (xedge -> xedge -> xedge)) merge
	end


	module EDGE_VISITOR(D0:MODELE_EDGE_VISITOR) =
	struct
		
		type memo = {
			man     : manager;
			extra   : D0.extra;
			calc    : edge -> D0.xedge;
			memo0	: (M.edge * M.leaf,  D0.xedge) MemoTable.t;
			memo1	: (M.edge * G.pnode, D0.xedge) MemoTable.t;
		}
		
		let makeman man extra hsize=
			let memo0, apply0 = MemoTable.make hsize
			and memo1, apply1 = MemoTable.make hsize in
			let rec calcrec (edge, gnode) = match gnode with
				| Leaf leaf -> apply0 fun0 (edge, leaf)
				| Node node -> apply1 fun1 (edge, node)
			and fun0 (edge, leaf) = D0.do_leaf extra edge leaf
			and fun1 (edge, node) = match D0.do_node extra edge with
				| MEdge xedge -> xedge
				| MNode merger ->
				(
					let edge0, edge1 = pull_node man node in
					merger (calcrec edge0) (calcrec edge1)
				)
			in
			({
				man  = man;
				extra = extra;
				calc = calcrec;
				memo0 = memo0;
				memo1 = memo1;
			}, calcrec)

		let newman man extra = makeman man extra 10000
		let calc man = man.calc
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "man edge leaf:"; MemoTable.dump_stats man.memo0];
			Tree.Node [Tree.Leaf "man edge node:"; MemoTable.dump_stats man.memo1];
		]
		
	end
	
	module type MODELE_CONS_VISITOR =
	sig
		type xedge
		type xresi
		type extra


		val do_edge: extra -> edge -> (xedge, xresi * edge) merge
		val push : extra -> xedge -> xedge -> xedge
		val compose : extra -> xresi -> xedge -> xedge
	end


	module CONS_VISITOR(D0:MODELE_CONS_VISITOR) =
	struct
		
		type memo = {
			man     : manager;
			extra   : D0.extra;
			calc    : edge -> D0.xedge;
			memo	: (edge,  D0.xedge) MemoTable.t;
		}
		
		let makeman man extra hsize=
			let memo, apply = MemoTable.make hsize in
			let rec calcrec edge = apply (fun edge ->
				match D0.do_edge extra edge with
				| MEdge xedge -> xedge
				| MNode (xresi, edge) ->
				(
					let edge0, edge1 = pull man edge in
					D0.compose extra xresi (D0.push extra (calcrec edge0) (calcrec edge1))
				)

			) edge
			in
			({
				man  = man;
				extra = extra;
				calc = calcrec;
				memo = memo;
			}, calcrec)

		let newman man extra = makeman man extra 10000
		let calc man = man.calc
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "man edge:"; MemoTable.dump_stats man.memo];
		]
		
	end

	module type MODELE_EUOP =
	(* External Unary OPerator *)
	sig
		include Map.OrderedType
		(* t = compact *)
		type transform
		type extern
		type t1
		type t2
		type extra
		val compose : extra -> transform -> extern -> extern
		val  decomp : extra -> G.tree -> t -> t1 * t2 * edge
		val  solver : extra -> t2 -> edge  -> (extern, (transform * (t * G.tree))) merge
		val  merger : extra -> t1 -> extern -> extern -> extern
	end
	
	module EUOP(D0:MODELE_EUOP) =
	(* External Unary OPerator *)
	struct
		
		type memo = {
			man  : manager;
			extra : D0.extra;
			calc : D0.t2 -> edge -> D0.extern;
			memo0	: (D0.t * M.leaf,  D0.extern) MemoTable.t;
			memo1	: (D0.t * G.pnode, D0.extern) MemoTable.t;
		}
		
		let makeman man extra hsize=
			let memo0, apply0 = MemoTable.make hsize
			and memo1, apply1 = MemoTable.make hsize in
			let pull = pull man in
			let rec calcrec t2 x = match D0.solver extra t2 x with
			| MEdge f -> f
			| MNode (t, (c, n)) -> D0.compose extra t (match n with
				| Leaf (l:M.leaf)	-> apply0 fun0 (c, l)
				| Node (n:G.pnode)	-> apply1 fun1 (c, n))
			and fun0 (c, l) = calc c (Leaf l)
			and fun1 (c, n) = calc c (Node n)
			and calc compact node =
				let (t1:D0.t1), (t2:D0.t2), (edge:edge) = D0.decomp extra node compact in
				let f0, f1 = pull edge in
				((D0.merger extra t1 (calcrec t2 f0) (calcrec t2 f1)):D0.extern)
			in
			({
				man  = man;
				extra = extra;
				calc = calcrec;
				memo0 = memo0;
				memo1 = memo1;
			}, calcrec)

		let newman man extra = makeman man extra 10000
		let calc man = man.calc
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "memo0:"; MemoTable.dump_stats man.memo0];
			Tree.Node [Tree.Leaf "memo1:"; MemoTable.dump_stats man.memo1];
		]
		
	end

	module MODELE_DUMP_EDGE : MODELE_EDGE_VISITOR with
			type xedge = Udag.StrTree.edge_t
		and type extra = Udag.StrTree.manager
	=
	struct
		type xedge = Udag.StrTree.edge_t
		type extra = Udag.StrTree.manager
		
		let dump_edge = match M.dump_edge with
			| None -> (fun _ -> Tree.Node [])
			| Some x -> x
		and dump_leaf = match M.dump_leaf with
			| None -> (fun _ -> Tree.Node [])
			| Some x -> x

		let do_leaf extra edge leaf =
			((dump_edge edge, Leaf (dump_leaf leaf)):Udag.StrTree.edge_t)

		let do_node extra edge =
			let merger edge0 edge1 =
				((dump_edge edge, Node (Udag.StrTree.push extra (Tree.Node [], [edge0; edge1]))):Udag.StrTree.edge_t)
			in ((MNode merger):(xedge, (xedge -> xedge -> xedge))merge)

	end

	module MODELE_TO_DOT_NODE : MODELE_NODE_VISITOR with
			type xedge = Udag.String.edge_t
		and type xnode = Udag.String.next_t
		and type extra = Udag.String.manager
	=
	struct
		type xedge = Udag.String.edge_t
		type xnode = Udag.String.next_t
		type extra = Udag.String.manager
		
		let dump_edge = match M.dot_of_edge with
			| None -> (fun _ -> "")
			| Some x -> x
		and dump_node = match M.dot_of_node with
			| None -> (fun _ -> "", "", "")
			| Some x -> x
		and dump_leaf = match M.dot_of_leaf with
			| None -> (fun _ -> "")
			| Some x -> x

		let do_leaf extra leaf =
			Leaf (dump_leaf leaf)

		let do_node (extra:extra) node =
			let tnode, tedge0, tedge1 = dump_node node in
			let merger next0 next1 =
				Node (Udag.String.push extra ((None, tnode), [(tedge0, next0); (tedge1, next1)]))
			in ((MNode merger):(xnode, (xnode -> xnode -> xnode))merge)

		let do_edge extra edge next =
			(dump_edge edge, next)

	end
	
	module TO_DOT_EDGE = NODE_VISITOR(MODELE_TO_DOT_NODE)
	
	let to_dot man strman : edge list -> MODELE_TO_DOT_NODE.xedge list =
		let man, calc = TO_DOT_EDGE.makeman man strman 10000 in
		List.map calc
	
	module MODELE_DUMP_NODE : MODELE_NODE_VISITOR with
			type xnode = Udag.StrTree.next_t
		and type xedge = Udag.StrTree.edge_t
		and type extra = Udag.StrTree.manager

	=
	struct
		type xnode = Udag.StrTree.next_t
		type xedge = Udag.StrTree.edge_t
		type extra = Udag.StrTree.manager

		let dump_leaf = match M.dump_leaf with
			| Some f -> f
			| None -> (fun _ -> assert false)

		let dump_node = match M.dump_node  with
			| Some f -> f
			| None -> (fun _ -> assert false)

		let dump_edge = match M.dump_edge with
			| Some f -> f
			| None -> (fun _ -> assert false)

		let do_leaf extra leaf = Leaf (dump_leaf leaf)
		let do_node extra node : (xnode, xnode -> xnode -> xnode) merge =
			let node = dump_node node in
			MNode(fun son0 son1 -> Node (Udag.StrTree.push extra (node, [(Tree.Node[], son0); (Tree.Node[], son1)])))

		let do_edge (extra:extra) edge son = (Tree.Node [dump_edge edge], son)

	end

	(* module DUMP_EDGE = EDGE_VISITOR(MODELE_DUMP_EDGE) *)
	module DUMP_NODE = NODE_VISITOR(MODELE_DUMP_NODE)

	let dump man dump_man : edge list -> MODELE_DUMP_NODE.xedge list =
		let man, calc = DUMP_NODE.makeman man dump_man 10000 in
		List.map calc
	
	module MODELE_LOAD_NODE : Udag.StrTree.MODELE_VISITOR with
			type xedge = edge
		and type xnode = edge
		and type extra = manager
	=
	struct
		type xedge = edge
		type xnode = edge
		type extra = manager

		let unop default = function Some x -> x | None -> default

		let load_leaf = unop (fun _ -> assert false) M.load_leaf
		let load_edge = unop (fun _ -> assert false) M.load_edge
		let load_node = unop (fun _ -> assert false) M.load_node

		let do_leaf _ leaf = load_leaf leaf
		let do_edge _ = function
			| Tree.Node [] -> (fun xnode -> xnode)
			| Tree.Node [edge] -> (fun xnode -> compose (load_edge edge) xnode)
			|  _ -> assert false
		let do_node man node xedgelist =
			let edgex, edgey = load_node node in
			match xedgelist with
			| [edge0; edge1] -> push man (compose edgex edge0) (compose edgey edge1)
			| _ -> assert false
	end

	module LOAD = Udag.StrTree.VISITOR(MODELE_LOAD_NODE)

	let load man dump_man =
		let man, calc = LOAD.newman dump_man man in
		List.map calc

	module type MODELE_IUOP =
	sig
		include Map.OrderedType
		(* t = compact *)
		type transform
		type t1
		type t2
		val compose : transform -> edge -> edge
		val  decomp : G.tree -> t -> t1 * t2 * edge
		val  solver : t2 -> edge -> (edge, (transform * (t*G.tree))) merge
		val  merger : t1 -> edge -> edge -> edge
	end


	module IUOP(D0:MODELE_IUOP) =
	(* Internal Unary OPerator *)
	struct
		module MY_MODELE_EUOP : MODELE_EUOP with
				type t = D0.t
			and type transform = D0.transform
			and type extern = edge
			and type t1 = D0.t1
			and type t2 = D0.t2
			and type extra = unit
		=
		struct
			type t = D0.t
			let compare = D0.compare
			type transform = D0.transform
			type extern = edge
			type t1 = D0.t1
			type t2 = D0.t2
			type extra = unit
			let compose () = D0.compose
			let  decomp () = D0.decomp
			let  solver () = D0.solver
			let  merger () = D0.merger
		end

		module MY_EUOP = EUOP(MY_MODELE_EUOP)

		type memo = MY_EUOP.memo

		let makeman	man = MY_EUOP.makeman man (():MY_MODELE_EUOP.extra)
		let newman man	= MY_EUOP.newman man ()
		let dump_stats	= MY_EUOP.dump_stats
		let calc		= MY_EUOP.calc
	end
			

	module type MODELE_IBOP =
	(* Internal Binary OPerator *)
	sig
		include Map.OrderedType
		(* t = compact *)
		type transform
		val compose : transform -> edge -> edge
		val  decomp : G.tree -> G.tree -> t -> edge * edge
		val  solver : (G.pnode -> G.ident) -> edge * edge -> (edge, (transform * (t*G.tree*G.tree))) merge
	end
	
	module IBOP(D0:MODELE_IBOP) =
	(* Internal Binary OPerator *)
	struct
		type memo = {
			man  : manager;
			calc : edge -> edge -> edge;
			memo	: (D0.t * G.tree * G.tree, edge) MemoTable.t;
		}

		type manager = memo
		
		let makeman man hsize=
			let memo, apply = MemoTable.make hsize in
			let push = push man
			and pull = pull man in
				let rec calcrec (x:edge) (y:edge) = match D0.solver G.get_ident (x, y) with
				| MEdge f -> f
				| MNode (t, (c, n1, n2)) -> D0.compose t (apply calc (c, n1, n2))
				and calc (compact, nx, ny) =
					let fx, fy = D0.decomp nx ny compact in
					let fx0, fx1 = pull fx
					and fy0, fy1 = pull fy in
					let f0 = calcrec fx0 fy0
					and f1 = calcrec fx1 fy1 in
					push f0 f1
			in
			{
				man  = man;
				calc = calcrec;
				memo = memo;
			}, calcrec
		let newman man = makeman man 10000
		let calc man = man.calc
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "memo:"; MemoTable.dump_stats man.memo];
		]
	end
	
	module type MODELE_IBOP_EVAL =
	(* Internal Binary OPerator *)
	sig
		type compact
		type residual
		type eval

		val  solver : (G.pnode -> G.ident) -> edge -> edge -> (
			M.edge * ((eval option) * G.tree),
			M.edge * ((M.edge * (eval option * G.tree)) * (M.edge * (eval option * G.tree))),
			residual * ( compact * ((eval option) * G.tree) * ((eval option) * G.tree))) merge3

		val solver' : (G.pnode -> G.ident) -> compact -> (edge, G.tree) merge -> (edge, G.tree) merge -> (
			M.edge * ((eval option) * G.tree),
			M.edge * ((M.edge * (eval option * G.tree)) * (M.edge * (eval option * G.tree))),
			residual * ( compact * ((eval option) * G.tree) * ((eval option) * G.tree))) merge3

		val eval : eval -> edge -> M.edge * ((eval option) * G.tree) (* apply the evaluation sequence on the descriptor *)

		val read : eval -> (unit, eval, eval, eval * eval) binpull (* read the first symbole of the evaluation sequence *)
		
		val  decomp : G.tree -> G.tree -> compact -> edge * edge
		val compose : residual -> edge -> edge
	end
	
	module IBOP_EVAL(D0:MODELE_IBOP_EVAL) =
	(* Internal Binary OPerator *)
	struct
		type memo = {
			man  : manager;
			calc : edge -> edge -> edge;
			memo : (D0.compact * G.tree * G.tree, edge) MemoTable.t;
			eval : (D0.eval    * G.tree         , edge) MemoTable.t;

		}

		type manager = memo
		
		let makeman man hsize =
			let memo, apply = MemoTable.make hsize in
			let memo_eval, apply_eval = MemoTable.make hsize in
			let push = push man
			and pull = pull man
			and pull_node = function
				| Leaf _		-> assert false
				| Node node	-> pull_node man node in
			let rec	read (set:D0.eval) (gtree:G.tree) =
				apply_eval (fun (set, gtree) -> match D0.read set with
				| MStop () -> assert false
				| Go0 set ->
				(
					let edge, _ = pull_node gtree in
					eval set edge
				)
				| Go1 set ->
				(
					let _, edge = pull_node gtree in
					eval set edge
				)
				| MPull (set0, set1) ->
				(
					let edge0, edge1 = pull_node gtree in
					push (eval set0 edge0) (eval set1 edge1)
				)
			) (set, gtree)
			and		eval (set:D0.eval) (edge:edge) =
				let edge, (opset, gtree) = D0.eval set edge in
				match opset with
				| None -> ((edge, gtree):edge)
				| Some set -> compose edge (read set gtree)
			in
			let eval_eog (edge, (opset, gtree)) : edge = match opset with
				| None     -> (edge, gtree)
				| Some set -> compose edge (read set gtree)
			in
			let rec calcrec (edgeX:edge) (edgeY:edge) = match D0.solver G.get_ident edgeX edgeY with
				| M3Edge eog -> eval_eog eog
				| M3Cons (edge, (eogX, eogY)) -> compose edge (push (eval_eog eogX) (eval_eog eogY))
				| M3Node (residual, (compact, nodeX, nodeY)) -> ((D0.compose residual (propa compact nodeX nodeY)):edge)
			and		propa compact (opevaX, gtreeX) (opevaY, gtreeY) = 
				if opevaX = None && opevaY = None
				then
				(
(*					print_string "{SUBDAG.IBOP_EVAL} opevax = None && opevay = None"; print_newline();*)
					apply calc (compact, gtreeX, gtreeY)
				)
				else
				(
(*					print_string "{SUBDAG.IBOP_EVAL} not(opevax = None && opevay = None)"; print_newline();*)
					let edgeX = match opevaX with None -> MNode gtreeX | Some set -> MEdge (read set gtreeX)
					and edgeY = match opevaY with None -> MNode gtreeY | Some set -> MEdge (read set gtreeY) in
					match D0.solver' G.get_ident compact edgeX edgeY with
					| M3Edge eog -> eval_eog eog
					| M3Cons (edge, (eogX, eogY)) -> compose edge (push (eval_eog eogX) (eval_eog eogY))
					| M3Node (residual, (compact, nodeX, nodeY)) -> D0.compose residual (propa compact nodeX nodeY)
				)
			and		calc (compact, gtreeX, gtreeY) =
				let fx, fy = D0.decomp gtreeX gtreeY compact in
				let fx0, fx1 = pull fx
				and fy0, fy1 = pull fy in
				let f0 = calcrec fx0 fy0
				and f1 = calcrec fx1 fy1 in
				push f0 f1
		in
		{
			man  = man;
			calc = calcrec;
			memo = memo;
			eval = memo_eval;
		}, calcrec
		let newman man = makeman man 10000
		let calc man = man.calc
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "memo:"; MemoTable.dump_stats man.memo];
			Tree.Node [Tree.Leaf "eval:"; MemoTable.dump_stats man.eval];
		]
	end
	
	module type MODELE_IBOP_BIDIR =
	(* Internal Binary OPerator with BIDIRectional propagation*)
	sig
		type compact
		type residual
		type eval

		type pnode = (M.leaf, eval option * G.pnode) gnode
		type pedge = M.edge * pnode

		val  solver : (G.pnode -> G.ident) -> pedge -> pedge ->
			(pedge, M.edge * (pedge * pedge), residual * (compact * pnode * pnode)) merge3

		val solver' : (G.pnode -> G.ident) -> compact -> (edge, G.tree) merge -> (edge, G.tree) merge ->
			(pedge, M.edge * (pedge * pedge), residual * (compact * pnode * pnode)) merge3

		val eval : eval -> pedge -> pedge (* apply the evaluation sequence on the descriptor *)

		val read : eval -> (unit, eval, eval, eval * eval) binpull (* read the first symbole of the evaluation sequence *)
		
		val  decomp : G.tree -> G.tree -> compact -> edge * edge
		val compose : residual -> edge -> edge
	end
	
	module IBOP_BIDIR(D0:MODELE_IBOP_BIDIR) =
	(* Internal Binary OPerator with BIDIRectional propagation*)
	struct
		type memo = {
			man  : manager;
			calc : edge -> edge -> edge;
			memo : (D0.compact * G.tree * G.tree, edge) MemoTable.t;
			eval : (D0.eval    * G.tree         , edge) MemoTable.t;
		}

		type manager = memo
		
		let makeman man hsize =
			let memo, apply = MemoTable.make hsize in
			let memo_eval, apply_eval = MemoTable.make hsize in
			let push = push man
			and pull = pull man
			and pull_node = function
				| Leaf _		-> assert false
				| Node node	-> pull_node man node in
			let rec	read (peval:D0.eval) (node:G.tree) =
				apply_eval (fun (peval, node) -> match D0.read peval with
				| MStop () -> assert false
				| Go0 peval -> eval peval (pull_node node |> fst)
				| Go1 peval -> eval peval (pull_node node |> snd)
				| MPull (peval0, peval1) ->
				(
					let edge0, edge1 = pull_node node in
					push (eval peval0 edge0) (eval peval1 edge1)
				)
			) (peval, node)
			and		eval (peval:D0.eval) (edge:edge) =
				eval_pedge (D0.eval peval (pedge_of_edge edge))
			and   eval_pedge (block, pnode) : edge = match pnode with
				| Leaf leaf -> (block, Leaf leaf)
				| Node (peval, node) -> match peval with
					| None -> (block, Node node)
					| Some peval -> compose block (read peval (Node node))
			in
			let rec calcrec (edgeX:edge) (edgeY:edge) : edge = match D0.solver G.get_ident (pedge_of_edge edgeX) (pedge_of_edge edgeY) with
				| M3Edge pedge -> eval_pedge pedge
				| M3Cons (block, (pedgeX, pedgeY)) -> compose block (push (eval_pedge pedgeX) (eval_pedge pedgeY))
				| M3Node (residual, (compact, pnodeX, pnodeY)) -> ((D0.compose residual (propa compact pnodeX pnodeY)):edge)
			and eval_pnode : D0.pnode -> (edge, G.tree) merge = function
				| Leaf leaf -> MNode (Leaf leaf)
				| Node (peval, node) -> match peval with
					| None -> MNode (Node node)
					| Some peval -> MEdge (read peval (Node node))
			and		propa compact pnodeX pnodeY = match pnodeX, pnodeY with
				| Node(Some _, _), _
				| _, Node(Some _, _) ->
				(
					match D0.solver' G.get_ident compact (eval_pnode pnodeX) (eval_pnode pnodeY) with
					| M3Edge pedge -> eval_pedge pedge
					| M3Cons (block, (pedgeX, pedgeY)) -> compose block (push (eval_pedge pedgeX) (eval_pedge pedgeY))
					| M3Node (residual, (compact, nodeX, nodeY)) -> D0.compose residual (propa compact nodeX nodeY)
				)
				| _ ->
				(
					let unp = function
						| Leaf leaf -> Leaf leaf
						| Node(none, node) -> (assert(none = None); Node node)
					in
					apply calc (compact, unp pnodeX, unp pnodeY)
				)
			and		calc (compact, nodeX, nodeY) =
				let fx, fy = D0.decomp nodeX nodeY compact in
				let fx0, fx1 = pull fx
				and fy0, fy1 = pull fy in
				let f0 = calcrec fx0 fy0
				and f1 = calcrec fx1 fy1 in
				push f0 f1
		in
		{
			man  = man;
			calc = calcrec;
			memo = memo;
			eval = memo_eval;
		}, calcrec
		let newman man = makeman man 10000
		let calc man = man.calc
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "memo:"; MemoTable.dump_stats man.memo];
			Tree.Node [Tree.Leaf "eval:"; MemoTable.dump_stats man.eval];
		]
	end

	module type MODELE_EVAL =
	sig
		type pars
		type back
		val pars : (G.pnode -> G.ident) -> pars -> edge -> (
			edge,
			back * pars,
			back * pars,
			back * pars * pars ) binpull
		val back : back -> edge -> edge
	end

	module EVAL(D0 : MODELE_EVAL) =
	struct
		type memo = {
			man  : manager;
			calc : D0.pars -> edge -> edge;
			memo : (D0.pars * edge, edge) MemoTable.t;
		}

		type manager = memo
		
		let makeman man hsize=
			let memo, apply = MemoTable.make hsize in
			let apply f x = f x in
			let push = push man
			and pull = function
				| Leaf _    -> assert false
				| Node node -> pull_node man node
			in
			let rec calcrec (pars:D0.pars) (e:edge) = apply (fun (pars, ((ex, ix) as e)) -> match D0.pars G.get_ident pars e with
				| MStop e -> e
				| Go0 (b, p) ->
				(
					let e0, _  = pull ix in
					D0.back b (calcrec p e0)
				)
				| Go1 (b, p) ->
				(
					let _ , e1 = pull ix in
					D0.back b (calcrec p e1)
				)
				| MPull (b, p0, p1) ->
				(
					let e0, e1 = pull ix in
					D0.back b (push (calcrec p0 e0) (calcrec p1 e1))
				)
			) (pars, e)
			in
			{
				man  = man;
				calc = calcrec;
				memo = memo;
			}, calcrec
		let newman man = makeman man 10000
		let calc man = man.calc
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "memo:"; MemoTable.dump_stats man.memo];
		]
	end

end
