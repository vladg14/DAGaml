module type MODELE = sig
	type leaf
	type edge
	type node
	type eval
	type tag
	
	type 't next'  = (leaf, 't) Utils.gnode
	type 't node'  = node * 't next' * 't next'
	type 't edge'  = edge * 't next'
	type 't edge'' = edge * 't node'

	type 't t_node = (unit, node, leaf, 't) Utils.node
	type 't t_edge = edge * 't t_node

	type 't p_node = (edge,  tag, leaf, eval option * 't) Utils.node
	type 't p_edge = edge * 't p_node

	type 't pt_node = ('t p_node, 't t_node) Utils.pt_node
	type 't pt_edge = edge * 't pt_node

	val push : ('t -> 'i) -> tag -> 't edge' -> 't edge' -> 't pt_edge
	val pull : ('t -> 'i) -> 't node' -> tag * 't edge' * 't edge'

	val arity_leaf : leaf -> int
	val arity_edge : edge -> int
	val arity_node : node -> int

	val edge_of_arity : int -> edge

	val eval_node : eval -> 't node' -> 't pt_edge
	val eval_edge : eval -> 't edge' -> 't pt_edge

	val compose : edge -> 't edge' -> 't edge'
	
	val dump_leaf : (leaf -> StrTree.tree) option
	val load_leaf : (StrTree.tree -> 't edge') option
	val dot_of_leaf : (leaf -> string) option

	val dump_edge : (edge -> StrTree.tree) option
	val load_edge : (StrTree.tree -> edge) option
	val dot_of_edge : (edge -> string) option
	
	val dump_node : (node -> StrTree.tree) option
	val load_node : (StrTree.tree -> tag * edge * edge) option
	val dot_of_node : (int -> node -> string) option

	val dot_of_tag : (tag -> string) option

end



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

		type 't gn = (leaf, 't) Utils.gnode
		type 't n = node * 't gn * 't gn
		type 't e = edge * 't gn

		let push = M0.push
		let pull = M0.pull
		let pull_node = M0.pull
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
	
	type manager = {
		gman : G.manager;
		push : M0.tag -> edge -> edge -> edge;
		walk_ptedge : G.ident M0.pt_edge -> edge;
		walk_pedge  : G.ident M0.p_edge  -> edge;
		walk_tnode  : G.ident M0.t_node  -> G.ident M0.next';
		walk_peval  : M0.eval -> G.pnode -> edge;
		memo_peval  : (M0.eval * G.pnode, edge) MemoTable.t;
	}


	let makeman size =
		let gman = G.makeman size in
		let memo_peval, apply_peval = MemoTable.make size in
		let rec push tag edge0 edge1 : G.ident M0.edge' =
			walk_ptedge (M.push G.get_ident tag edge0 edge1)
		and     walk_ptedge (edge, ptnode) : G.ident M0.edge' = match ptnode with
			| Utils.PTree node -> walk_pedge (edge, node)
			| Utils.TTree node -> (edge, walk_tnode node)
		and     walk_pedge (edge, pnode) : G.ident M0.edge' = match pnode with
			| Utils.TNode (tag, (pedge0, pedge1)) ->
			(
				let edge0 = walk_pedge pedge0
				and edge1 = walk_pedge pedge1 in
				M0.compose edge (push tag edge0 edge1)
			)
			| Utils.TLeaf leaf ->
			(
				(edge, Utils.Leaf leaf)
			)
			| Utils.TLink (peval, node) ->
			( match peval with
				| None -> (edge, Utils.Node node)
				| Some peval -> M0.compose edge (walk_peval peval node)
			)
		and     walk_peval peval node = apply_peval
			(fun (peval, node) -> walk_ptedge (M0.eval_node peval (G.pull gman node))) (peval, node)
		and     walk_tnode : G.ident M0.t_node -> G.ident M0.next' = function
			| Utils.TNode (node, (((), tnode0), ((), tnode1))) ->
			(
				let next0 = walk_tnode tnode0
				and next1 = walk_tnode tnode1 in
				let node = G.push gman (node, next0, next1) in
				Utils.Node node
			)
			| Utils.TLeaf leaf -> Utils.Leaf leaf
			| Utils.TLink node -> Utils.Node node	
		in
		{gman; push; walk_ptedge; walk_pedge; walk_tnode; walk_peval; memo_peval}
  
	let default_newman_hsize = 10000
		
	let newman () = makeman default_newman_hsize

	let push man = man.push

	let push_leaf (e:M.edge) (l:M.leaf) = ((e, ((Utils.Leaf l):G.tree)):edge)
	let pull man (n:G.pnode) = M.pull G.get_ident (G.pull man.gman n)

	let compose = M.compose

	let dump_stats man = Tree.Node [
		G.dump_stats man.gman;
		Tree.Node [Tree.Leaf "#eval: "; MemoTable.dump_stats man.memo_peval]
	]
	

	module type MODELE_NODE_VISITOR =
	sig
		type xnode
		type xedge
		type extra

		val do_leaf : extra -> M.leaf -> xnode 
		val do_node : extra -> M.node -> (xnode, xnode -> xnode -> xnode) Utils.merge
		val do_edge : extra -> M.edge -> xnode -> xedge

	end
	

	module NODE_VISITOR(D0:MODELE_NODE_VISITOR)=
	struct
		type memo = {
			man  : manager;
			extra : D0.extra;
			calc : edge -> D0.xedge;
			memoLeaf: (M.leaf,  D0.xnode) MemoTable.t;
			memoEdge: (edge,    D0.xedge) MemoTable.t;
			memoNode: (G.pnode, D0.xnode) MemoTable.t;
		}

		type manager = memo
		
		let makeman man extra hsize =
			let memoLeaf, applyLeaf = MemoTable.make hsize
			and memoEdge, applyEdge = MemoTable.make hsize
			and memoNode, applyNode = MemoTable.make hsize in
			let rec calcrec = function
				| Utils.Leaf leaf -> calcleaf leaf
				| Utils.Node node -> calcnode node
			and		calcedge edge = applyEdge (fun (edge, gnode) -> D0.do_edge extra edge (calcrec gnode)) edge
			and		calcleaf leaf = applyLeaf (D0.do_leaf extra) leaf
			and		calcnode node = applyNode (fun node ->
				let (node:M.node), (nx:G.tree), (ny:G.tree) = G.pull man.gman node in
				match D0.do_node extra node with
				| Utils.MEdge xnode -> xnode
				| Utils.MNode merger -> merger (calcrec nx) (calcrec ny)) node
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
		val do_node: extra -> M.edge -> (xedge, (M.tag -> xedge -> xedge -> xedge)) Utils.merge
	end


	module EDGE_VISITOR(D0:MODELE_EDGE_VISITOR) =
	struct
		
		type memo = {
			man     : manager;
			extra   : D0.extra;
			calc    : edge -> D0.xedge;
			memo0	: (M.edge * M.leaf, D0.xedge) MemoTable.t;
			memo1	: (M.edge * G.pnode, D0.xedge) MemoTable.t;
		}

		type manager = memo
		
		let makeman man extra hsize=
			let memo0, apply0 = MemoTable.make hsize
			and memo1, apply1 = MemoTable.make hsize in
			let rec calcrec (edge, gnode) = match gnode with
				| Utils.Leaf leaf -> apply0 fun0 (edge, leaf)
				| Utils.Node node -> apply1 fun1 (edge, node)
			and fun0 (edge, leaf) = D0.do_leaf extra edge leaf
			and fun1 (edge, node) = match D0.do_node extra edge with
				| Utils.MEdge xedge -> xedge
				| Utils.MNode merger ->
				(
					let tag, edge0, edge1 = pull man node in
					merger tag (calcrec edge0) (calcrec edge1)
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

		let do_leaf extra leaf = Utils.Leaf (dump_leaf leaf)
		let do_node extra node : (xnode, xnode -> xnode -> xnode) Utils.merge =
			let node = dump_node node in
			Utils.MNode(fun son0 son1 -> Utils.Node (Udag.StrTree.push extra (node, [(Tree.Node[], son0); (Tree.Node[], son1)])))

		let do_edge (extra:extra) edge son = (Tree.Node [dump_edge edge], son)

	end

	(* module DUMP_EDGE = NODE_VISITOR(MODELE_DUMP_EDGE) *)
	module DUMP_NODE = NODE_VISITOR(MODELE_DUMP_NODE)

	let dump man dump_man : edge list -> MODELE_DUMP_NODE.xedge list =
		let man, calc = DUMP_NODE.makeman man dump_man 10000 in
		List.map calc

	module MODELE_TO_DOT_EDGE : MODELE_EDGE_VISITOR with
			type xedge = Udag.String.edge_t
		and type extra = Udag.String.manager
	=
	struct
		type xedge = Udag.String.edge_t
		type extra = Udag.String.manager
		
		let dump_edge = match M.dot_of_edge with
			| None -> (fun _ -> "")
			| Some x -> x
		and dump_leaf = match M.dot_of_leaf with
			| None -> (fun _ -> "")
			| Some x -> x
		and dump_tag = match M.dot_of_tag with
			| None -> (fun _ -> "")
			| Some x -> x

		let do_leaf extra edge leaf =
			((dump_edge edge, Utils.Leaf (dump_leaf leaf)):Udag.String.edge_t)

		let do_node extra edge =
			let merger tag edge0 edge1 =
				((dump_edge edge, Utils.Node (Udag.String.push extra ((None, dump_tag tag), [edge0; edge1]))):Udag.String.edge_t)
			in ((Utils.MNode merger):(xedge, (M.tag -> xedge -> xedge -> xedge))Utils.merge)

	end
	
	module TO_DOT_EDGE = EDGE_VISITOR(MODELE_TO_DOT_EDGE)
	
	let to_dot man strman : edge list -> MODELE_TO_DOT_EDGE.xedge list =
		let man, calc = TO_DOT_EDGE.makeman man strman 10000 in
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
			let tag, edgex, edgey = load_node node in
			match xedgelist with
			| [edge0; edge1] -> push man tag (compose edgex edge0) (compose edgey edge1)
			| _ -> assert false
	end

	module LOAD = Udag.StrTree.VISITOR(MODELE_LOAD_NODE)

	let load man dump_man =
		let man, calc = LOAD.newman dump_man man in
		List.map calc
	
	module type MODELE_IUOP =
	sig
		type eval

		val eval : eval -> edge -> (
			edge,
			M.edge * (eval * G.pnode)
		) Utils.merge
		val read : eval -> M.tag -> (
			edge,				(* MStop *)
			eval,				(*  Go0  *)
			eval,				(*  Go1  *)
			M.tag * eval * eval	(* MPull *)
		) Utils.binpull
	end

	module IUOP(D0:MODELE_IUOP) =
	struct
		type memo = {
			man     : manager;
			calc    : D0.eval -> edge -> edge;
			memo    : (D0.eval * G.pnode, edge) MemoTable.t;
		}

		type manager = memo
		
		let makeman man extra hsize=
			let memo, apply = MemoTable.make hsize in
			let rec calcrec mess edge = match D0.eval mess edge with
				| Utils.MEdge edge -> edge
				| Utils.MNode (medge, (mess, node)) -> compose medge (apply calc (mess, node))
			and		calc (mess, node) =
				let tag, edgeX, edgeY = pull man node in
				match D0.read mess tag with
				| Utils.MStop edge -> edge
				| Utils.Go0 eval -> calcrec eval edgeX
				| Utils.Go1 eval -> calcrec eval edgeY
				| Utils.MPull (tag, messX, messY) -> push man tag (calcrec messX edgeX) (calcrec messY edgeY)
			in
			({
				man  = man;
				calc = calcrec;
				memo = memo;
			}, calcrec)

		let newman man extra = makeman man extra 10000
		let calc man = man.calc
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "memo:"; MemoTable.dump_stats man.memo];
		]
		
	end
	
	module type MODELE_IUOP2 =
	sig
		type eval

		val eval : eval -> edge -> (
			edge,
			M.edge * (eval * G.pnode)
		) Utils.merge
		val read : eval -> M.tag -> (
			edge,				(* MStop *)
			eval,				(*  Go0  *)
			eval,				(*  Go1  *)
			M.tag * eval * eval	(* MPull *)
		) Utils.binpull

		val solver : (G.pnode -> G.ident) -> M.tag -> edge -> edge -> (
			M.edge * (eval option * G.tree),
			M.edge * (M.tag * (M.edge * (eval option * G.tree)) * (M.edge * (eval option * G.tree)))
		) Utils.merge

	end

	module IUOP2(D0:MODELE_IUOP2) =
	struct
		type memo = {
			man     : manager;
			calc    : edge -> edge;
			eval    : D0.eval -> edge -> edge;
			meval   : (D0.eval * G.pnode  , edge) MemoTable.t;
			mcalc   : (M.tag * edge * edge, edge) MemoTable.t;
		}

		type manager = memo
		
		let makeman man hsize=
			let memo, apply = MemoTable.make hsize in
			let memo_eval, apply_eval = MemoTable.make hsize in
			let rec eval_edge mess edge = match D0.eval mess edge with
				| Utils.MEdge edge -> edge
				| Utils.MNode (medge, (mess, node)) -> compose medge (apply_eval eval_node (mess, node))
			and		eval_node (mess, node) =
				let tag, edgeX, edgeY = pull man node in
				match D0.read mess tag with
				| Utils.MStop edge -> edge
				| Utils.Go0 eval -> eval_edge eval edgeX
				| Utils.Go1 eval -> eval_edge eval edgeY
				| Utils.MPull (tag, messX, messY) -> solve_node (tag, (eval_edge messX edgeX), (eval_edge messY edgeY))
			and		eval_eog (edge, (opset, gtree)) : edge = match opset with
				| None -> (edge, gtree)
				| Some set -> compose edge (match gtree with Utils.Leaf _ -> assert false | Utils.Node node -> (apply_eval eval_node (set, node)))
			and		solve_node node : edge = apply (fun (tag, edgeX, edgeY) -> match D0.solver G.get_ident tag edgeX edgeY with
				| Utils.MEdge eog -> eval_eog eog
				| Utils.MNode (edge, (tag, (edgeX, (None, gtreeX)), (edgeY, (None, gtreeY)))) -> compose edge (push man tag (edgeX, gtreeX) (edgeY, gtreeY))
				| Utils.MNode (edge, (tag, eogX, eogY)) -> compose edge (solve_node (tag, eval_eog eogX, eval_eog eogY))) node
			in
			let rec solve_pnode (pnode:G.pnode) =
				let tag, edgeX, edgeY = pull man pnode in
				solve_node (tag, solve_edge edgeX, solve_edge edgeY)
			and		solve_edge (edge, gtree) : edge = match gtree with
				| Utils.Leaf leaf -> (((edge:M.edge), Utils.Leaf leaf):edge)
				| Utils.Node (node:G.pnode) -> compose (edge:M.edge) (solve_pnode node)
			in
			({
				man  = man;
				calc = solve_edge;
				eval = eval_edge;
				meval = memo_eval;
				mcalc = memo;
			}, solve_edge)

		let newman man = makeman man 10000
		let calc man = man.calc
		let eval man = man.eval
		let dump_stats man = Tree.Node [
			Tree.Node [Tree.Leaf "eval:"; MemoTable.dump_stats man.meval];
			Tree.Node [Tree.Leaf "memo:"; MemoTable.dump_stats man.mcalc];
		]
	end
	
(*
	module type MODELE_IUOP_EVAL =
	sig
	end

	module IUOP_EVAL(D0:MODELE_IUOP_EVAL) =
	struct
	end
*)

end
