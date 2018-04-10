(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

let strdump_node = STD.bitv_hexa
let strload_node = Extra.(STL.bitv_hexa >> Cpx2DumpLoad.binload_node >> Cpx2Utils.block_split)

let strdump_tacx = STD.bitv_hexa
let strload_tacx = Extra.(STL.bitv_hexa >> Cpx2DumpLoad.binload_tacx >> Cpx2Utils.tacx_split)

let strdump_edge = Extra.(Cpx2DumpLoad.bindump_block' >> STD.bitv_hexa)
let strload_edge = Extra.(STL.bitv_hexa >> Cpx2DumpLoad.binload_block')

let dot_of_edge_aux color block =
	"[label = \""^(Cpx2DumpLoad.pretty_of_block block)^"\"; color=\""^color^"\"];"

let dot_of_edge = dot_of_edge_aux "black"

let dot_of_node node =
	let e0, e1 = node |> Cpx2DumpLoad.binload_node |> Cpx2Utils.block_split in
	"", (dot_of_edge_aux "red" e0), (dot_of_edge_aux "blue" e1)

let default_leaf = Cpx2Types.({neg = false; arity = 0; block = C0}, Utils.Leaf ())

let strdump_leaf = (fun () -> Tree.Node [])
let strload_leaf = (function Tree.Node [] -> default_leaf | _ -> assert false)

module GroBdd_M : Subdag.MODELE with
		type node = Bitv.t
	and	type edge = Cpx2Types.edge_state
	and type leaf = unit
=
struct
	
	type node = Bitv.t
	type edge = Cpx2Types.edge_state
	type leaf = unit

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push : ('t -> 'i) -> 't e -> 't e -> ('t e, edge * 't n) Utils.merge = Cpx2Gops.node_push_cons
	let pull : ('t -> 'i) -> 't e -> ('t e * 't e, 't n -> ('t e * 't e)) Utils.merge = fun _ edge -> match Cpx2Gops.node_pull edge with
		| Utils.MEdge e -> Utils.MEdge e
		| Utils.MNode f -> Utils.MNode (fun (node, i1, i2) -> f (Cpx2DumpLoad.binload_node node, i1, i2))
	let compose = Cpx2Utils.compose_edge
	
	let pull_node _ (n, i0, i1) = Cpx2Utils.node_split (Cpx2DumpLoad.binload_node n, i0, i1)
	
	let dump_node   = Some strdump_node
	let load_node   = Some strload_node
	let dot_of_node = Some dot_of_node
	
	let dump_edge   = Some strdump_edge
	let load_edge   = Some strload_edge
	let dot_of_edge = Some dot_of_edge

	let dump_leaf   = Some strdump_leaf
	let load_leaf   = Some strload_leaf
	let dot_of_leaf = Some (function () -> "[label = \"0\"];")
end

module GroBdd =
struct
	include Subdag.MODULE(GroBdd_M)
	let dumpfile man edges target =
		let strman = Udag.STree.newman() in
		let stredges = dump man strman edges in
		Udag.STree.dumpfile strman stredges target;
		strman
	
	let loadfile ?man target =
		let strman, stredges = Udag.STree.loadfile target in
		let man = match man with None -> newman () | Some man -> man in
		let edges = load man strman stredges in
		man, edges
end

let newman = GroBdd.newman

let make_const b n = GroBdd.push_leaf (Cpx2Utils.make_block_C0 b n) ();;

let make_ident man b n = GroBdd.push man (make_const b n) (make_const (not b) n);;

let arity block = block.Cpx2Types.arity

let push_pass = Cpx2Utils.push_P_block

let no = Cpx2Utils.neg

let is_root = Cpx2Utils.edge_is_const

let get_root = Cpx2Utils.get_root

let (=??) (ex, ix) (ey, iy) = match ix, iy with
	| Utils.Leaf (), Utils.Leaf () -> ex = ey
	| Utils.Node nx, Utils.Node ny -> (nx == ny) && (ex = ey)
	| _ -> false

module AND_M2 =
(* Internal Binary OPerator with BIDIRectional propagation*)
struct
	type compact = Bitv.t
	type residual = Cpx2Types.block
	type eval = bool option list

	type pnode = (GroBdd.M.leaf, eval option * GroBdd.G.pnode) Utils.gnode
	type pedge = GroBdd.M.edge * pnode

	let solver gid pedge0 pedge1 =
		match Cpx2Gops.solve_and pedge0 pedge1 with
		| Utils.M3Edge pedge -> Utils.M3Edge pedge
		| Utils.M3Cons pcons -> Utils.M3Cons pcons
		| Utils.M3Node (blockC, (node, pnode0, pnode1)) ->
			Utils.M3Node (blockC, (Cpx2DumpLoad.bindump_node node, pnode0, pnode1))


	let solver' gid compact mnode0 mnode1 = 
		let block0C, block1C = Cpx2DumpLoad.binload_node compact in
		let fuse block = function
			| Utils.MEdge edge -> Cpx2Utils.compose_edge block edge
			| Utils.MNode node -> Cpx2Utils.reduce_edge (block, node)
		in
		let pedge0 : pedge = Cpx2Gops.pedge_of_edge (fuse block0C mnode0)
		and pedge1 : pedge = Cpx2Gops.pedge_of_edge (fuse block1C mnode1) in
		solver gid pedge0 pedge1

	let eval (set : eval) (pedge : pedge) : pedge = Cpx2Utils.assign_pedge' set pedge

	let read = function [] -> assert false | head::tail -> match head with
			| None		 -> Utils.MPull (tail, tail)
			| Some false -> Utils.Go0 tail
			| Some true  -> Utils.Go1 tail

	let decomp node0 node1 compact =
		let block0, block1 = Cpx2DumpLoad.binload_node compact in
		(block0, node0), (block1, node1)
	
	let compose = Cpx2Utils.compose_edge
end

module AND = GroBdd.IBOP_BIDIR(AND_M2)

module XOR_M2 =
(* Internal Binary OPerator with BIDIRectional propagation*)
struct
	type compact = Bitv.t
	type residual = Cpx2Types.block
	type eval = bool option list

	type pnode = (GroBdd.M.leaf, eval option * GroBdd.G.pnode) Utils.gnode
	type pedge = GroBdd.M.edge * pnode

	let solver gid pedge0 pedge1 =
		match Cpx2Gops.solve_xor pedge0 pedge1 with
		| Utils.M3Edge pedge -> Utils.M3Edge pedge
		| Utils.M3Cons pcons -> Utils.M3Cons pcons
		| Utils.M3Node (blockC, (node, pnode0, pnode1)) ->
			Utils.M3Node (blockC, (Cpx2DumpLoad.bindump_node node, pnode0, pnode1))


	let solver' gid compact mnode0 mnode1 =
		let block0C, block1C = Cpx2DumpLoad.binload_node compact in
		let fuse block = function
			| Utils.MEdge edge -> Cpx2Utils.compose_edge block edge
			| Utils.MNode node -> Cpx2Utils.reduce_edge (block, node)
		in
		let pedge0 = Cpx2Gops.pedge_of_edge (fuse block0C mnode0)
		and pedge1 = Cpx2Gops.pedge_of_edge (fuse block1C mnode1) in
		solver gid pedge0 pedge1

	let eval (set : eval) (pedge : pedge) : pedge = Cpx2Utils.assign_pedge' set pedge

	let read = function [] -> assert false | head::tail -> match head with
			| None		 -> Utils.MPull (tail, tail)
			| Some false -> Utils.Go0 tail
			| Some true  -> Utils.Go1 tail

	let decomp node0 node1 compact =
		let block0, block1 = Cpx2DumpLoad.binload_node compact in
		(block0, node0), (block1, node1)

	let compose = Cpx2Utils.compose_edge
end

module XOR = GroBdd.IBOP_BIDIR(XOR_M2)


module TACX_M : TaggedSubdagPropa.MODELE with
		type node = Bitv.t
	and	type edge = Cpx2Types.edge_state
	and type leaf = unit
	and type eval = bool option list
	and type tag  = TacxTypes.tag
=
struct
	
	type node = Bitv.t
	type edge = Cpx2Types.edge_state
	type leaf = unit
	type eval = bool option list
	type tag  = TacxTypes.tag

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

	let ptedge_of_pedge (block, pnode) = (block, match pnode with
			| Utils.Leaf () ->
				Utils.TTree(Utils.TLeaf())
			| Utils.Node (None, node) ->
				Utils.TTree(Utils.TLink node)
			| Utils.Node (peval, node) ->
				Utils.PTree(Utils.TLink(peval, node))
		)
		

	let push gid (tag : TacxTypes.tag) (edge0 : _ edge') (edge1 : _ edge') =
		let pedge0 = Cpx2Gops.pedge_of_edge edge0
		and pedge1 = Cpx2Gops.pedge_of_edge edge1 in
		match Cpx2Gops.tacx_propa gid tag pedge0 pedge1 with
		| Utils.MEdge pedge -> ptedge_of_pedge pedge
		| Utils.MNode (block, (tag, (block0, pnode0), (block1, pnode1))) ->
		(
			let tnext_of_pnode = function
				| Utils.Leaf () -> Utils.TLeaf ()
				| Utils.Node (none, node) ->
				(	
					assert(none = None);
					Utils.TLink node
				)
			in
			let pnext_of_pnode = function
				| Utils.Leaf () -> Utils.TLeaf ()
				| Utils.Node (peval, node) -> Utils.TLink(peval, node)
			in
			(block, match pnode0, pnode1 with
				| Utils.Node (Some _, _), _
				| _, Utils.Node (Some _, _) ->
				(
					let pnext0 = pnext_of_pnode pnode0
					and pnext1 = pnext_of_pnode pnode1 in
					Utils.PTree(Utils.TNode(tag, ((block0, pnext0), (block1, pnext1))))
				)
				| _ ->
				(
					let tnext0 = tnext_of_pnode pnode0
					and tnext1 = tnext_of_pnode pnode1 in
					let node01 = Cpx2DumpLoad.bindump_tacx (tag, (block0, block1)) in
					Utils.TTree(Utils.TNode(node01, (((), tnext0), ((), tnext1))))
				)
			)
		)
		
	let pull gid (node, pnode0, pnode1) =
		let tag, (block0, block1) = Cpx2DumpLoad.binload_tacx node in
		(tag, (block0, pnode0), (block1, pnode1))
	let compose = Cpx2Utils.compose_edge


	let arity_leaf () = 0
	let arity_edge edge = edge.Cpx2Types.arity
	let arity_node node =
		let _, (block0, block1) = Cpx2DumpLoad.binload_tacx node in
		assert(block0.Cpx2Types.arity = block1.Cpx2Types.arity);
		block0.Cpx2Types.arity

	let edge_of_arity arity = Cpx2Utils.make_block_S false arity
	
	let eval_edge (peval : eval) (edge : _ edge') : (_ pt_edge) = ptedge_of_pedge (Cpx2Utils.assign_pedge' peval (Cpx2Gops.pedge_of_edge edge))


	let eval_node peval (node, node0, node1) =
		let tnode_of_node = function
			| Utils.Leaf () -> Utils.TLeaf ()
			| Utils.Node node -> Utils.TLink node
		in
(*		let pnode_of_node peval = function
			| Utils.Leaf () -> (assert(peval = []); Utils.TLeaf ())
			| Utils.Node node -> Utils.TLink (peval, node)
		in *)
		let pnode_of_pnode = function
			| Utils.Leaf () -> Utils.TLeaf ()
			| Utils.Node node -> Utils.TLink node
		in
		let pedge_of_pedge (block, pnode) = (block, pnode_of_pnode pnode) in
		if List.for_all (function None -> true | _ -> false) peval
		then (edge_of_arity(arity_node node), Utils.TTree(Utils.TNode(node, (((), tnode_of_node node0), ((), tnode_of_node node1)))))
		else
		(
			let tag, (block0, block1) = Cpx2DumpLoad.binload_tacx node in
			let arity = block0.Cpx2Types.arity in
			assert(arity = block1.Cpx2Types.arity);
			match tag with
			| TacxTypes.And
			| TacxTypes.Xor ->
			(
				let blockC = edge_of_arity arity in
				let pnode0 = Cpx2Gops.pnode_of_node node0
				and pnode1 = Cpx2Gops.pnode_of_node node1 in
				let pedge0 = Cpx2Utils.assign_pedge' peval (block0, pnode0)
				and pedge1 = Cpx2Utils.assign_pedge' peval (block1, pnode1) in
				let pedge0 = pedge_of_pedge pedge0
				and pedge1 = pedge_of_pedge pedge1 in
				(blockC, Utils.PTree(Utils.TNode(tag, (pedge0, pedge1))))
			)
			| TacxTypes.Cons -> match peval with
				| [] -> assert false
				| head::tail ->
				(
					if List.for_all (function None -> true | _ -> false) tail
					then
					(match head with
						| None -> assert false
						| Some path -> if path
						then (block1, Utils.TTree(tnode_of_node node1))
						else (block0, Utils.TTree(tnode_of_node node0))
					)
					else
					(
						let fmap  (block, node) = (block, node) |> Cpx2Gops.pedge_of_edge |> (Cpx2Utils.assign_pedge' tail) |>  pedge_of_pedge in
						let fmap' (block, node) = (block, node) |> Cpx2Gops.pedge_of_edge |> (Cpx2Utils.assign_pedge' tail) |> ptedge_of_pedge in
						match head with
						| None -> (edge_of_arity (1+(MyList.count (function None -> true | _ -> false) tail)), Utils.PTree(Utils.TNode(TacxTypes.Cons, ( fmap (block0, node0) , fmap (block1, node1)))))
						| Some path -> if path
						then (fmap' (block1, node1))
						else (fmap' (block0, node0))
					)
			)
	)
	
	let dump_node   = Some strdump_tacx
	let load_node   = Some strload_tacx
	let dot_of_node = None
	
	let dump_edge   = Some strdump_edge
	let load_edge   = Some strload_edge
	let dot_of_edge = Some dot_of_edge

	let dump_leaf   = Some strdump_leaf
	let load_leaf   = Some strload_leaf
	let dot_of_leaf = Some (fun () -> "0")

	let dot_of_tag = Some Extra.(TacxTypes.strdump_tag >> (fun x -> "[label = \""^x^"\"];"))
end

module TACX =
struct
	include TaggedSubdagPropa.MODULE(TACX_M)
	let dumpfile man edges target =
		let strman = Udag.STree.newman() in
		let stredges = dump man strman edges in
		Udag.STree.dumpfile strman stredges target
	
	let loadfile target =
		let strman, stredges = Udag.STree.loadfile target in
		let man = newman () in
		let edges = load man strman stredges in
		man, edges
end

let ( *! ) man x y = TACX.push man TacxTypes.Cons x y
let ( &! ) man x y = TACX.push man TacxTypes.And x y
and ( ^! ) man x y = TACX.push man TacxTypes.Xor x y

module EVAL =
struct
	module EVAL_VISITOR : TACX.MODELE_NODE_VISITOR with
			type xnode = GroBdd.edge
		and type xedge = GroBdd.edge
		and type extra = (GroBdd.edge -> GroBdd.edge -> GroBdd.edge) * (GroBdd.edge -> GroBdd.edge -> GroBdd.edge) * (GroBdd.edge -> GroBdd.edge -> GroBdd.edge) =
	struct
		type xnode = GroBdd.edge
		type xedge = GroBdd.edge
		type cons = xedge -> xedge -> xedge
		type extra = cons * cons * cons (* (a, c, x) *)

		let do_leaf _ () = default_leaf
		let do_node (a, c, x) = Extra.(Cpx2DumpLoad.binload_tacx >> Cpx2Utils.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let merge = NniTypes.(match tag with TacxTypes.And -> a | TacxTypes.Cons -> c | TacxTypes.Xor -> x) in
			Utils.MNode (fun nodeX nodeY -> merge (Cpx2Utils.compose_edge edgeX nodeX) (Cpx2Utils.compose_edge edgeY nodeY))))
		let do_edge _ = Cpx2Utils.compose_edge
	end

	module EVAL = TACX.NODE_VISITOR(EVAL_VISITOR)

	type manager = {
		grobdd : GroBdd.manager;
		andman : AND.manager;
		xorman : XOR.manager;
		theman : EVAL.manager;
		calc   : TACX.edge -> GroBdd.edge
	}

	let newman tacx man =
		let c = GroBdd.push man in
		let aman, a = AND.newman man
		and xman, x = XOR.newman man in
		let theman, calc = EVAL.newman tacx (a, c, x) in
		{
			grobdd = man;
			andman = aman;
			xorman = xman;
			theman = theman;
			calc = calc
		}, List.map calc
	
	let dump_stats man = Tree.Node [
		Tree.Node [Tree.Leaf "andman:"; AND.dump_stats man.andman];
		Tree.Node [Tree.Leaf "xorman:"; XOR.dump_stats man.xorman];
		Tree.Node [Tree.Leaf "theman:"; EVAL.dump_stats man.theman];
	]


end

module PURE_OF_TACX =
struct
	module VISITOR =
	struct
		type xnode = GroBdd.edge
		type xedge = GroBdd.edge
		type cons = xedge -> xedge -> xedge
		type extra = cons * cons * cons (* (a, c, x) *)

		let do_leaf _ () = default_leaf
		let do_node (a, c, x) = Extra.(Cpx2DumpLoad.binload_tacx >> Cpx2Utils.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let merge = TacxTypes.(match tag with And -> a | Cons -> c | Xor -> x) in
			Utils.MNode (fun nodeX nodeY -> merge (Cpx2Utils.compose_edge edgeX nodeX) (Cpx2Utils.compose_edge edgeY nodeY))))
		let do_edge _ = Cpx2Utils.compose_edge
	end

	module EVAL = TACX.NODE_VISITOR(VISITOR)

	type manager = {
		grobdd : GroBdd.manager;
		andman : AND.manager;
		xorman : XOR.manager;
		theman : EVAL.manager;
		calc   : TACX.edge -> GroBdd.edge
	}

	let newman tacx man =
		let c = GroBdd.push man in
		let aman, a = AND.newman man
		and xman, x = XOR.newman man in
		let theman, calc = EVAL.newman tacx (a, c, x) in
		{
			grobdd = man;
			andman = aman;
			xorman = xman;
			theman = theman;
			calc = calc
		}, List.map calc
	
	let dump_stats man = Tree.Node [
		Tree.Node [Tree.Leaf "andman:"; AND.dump_stats man.andman];
		Tree.Node [Tree.Leaf "xorman:"; XOR.dump_stats man.xorman];
		Tree.Node [Tree.Leaf "theman:"; EVAL.dump_stats man.theman];
	]


end

module PURE_OF_CP =
struct
	module EVAL_VISITOR =
	struct
		type xnode = GroBdd.edge
		type xedge = GroBdd.edge
		type cons = xedge -> xedge -> xedge
		type extra = GroBdd.manager

		let do_leaf _ () = default_leaf
		let mu_cpx_of_cp = function
			| CpTypes.S -> Cpx2Types.S
			| CpTypes.P -> Cpx2Types.P
		let cpx_of_cp (b, u) = Cpx2Utils.spx_liste_to_block b false (List.map mu_cpx_of_cp u)
		let do_node extra = Extra.(CpGops.binload_node >> CpGops.node_split >> (fun (edgeX, edgeY) ->
			let edgeX = cpx_of_cp edgeX
			and edgeY = cpx_of_cp edgeY in
			let result = Utils.MNode Cpx2Gops.(fun nodeX nodeY ->
				GroBdd.push extra (Cpx2Utils.compose_edge edgeX nodeX) (Cpx2Utils.compose_edge edgeY nodeY)) in
			(result : (xnode, xnode -> xnode -> xnode) Utils.merge)))

		let do_edge _ e = Cpx2Utils.compose_edge (cpx_of_cp e)
	end

	module EVAL = Cp.GroBdd.NODE_VISITOR(EVAL_VISITOR)

	type manager = {
		man_cp : Cp.GroBdd.manager;
		mannni : GroBdd.manager;
		theman : EVAL.manager;
		calc   : Cp.GroBdd.edge -> GroBdd.edge
	}

	let newman tacx_cp tacx_nni =
		let theman, calc = EVAL.newman tacx_cp tacx_nni in
		{
			man_cp = tacx_cp;
			mannni = tacx_nni;
			theman = theman;
			calc   = calc;
		}, List.map calc
	
	let dump_stats man = Tree.Node [
		Tree.Node [Tree.Leaf "theman:"; EVAL.dump_stats man.theman];
	]


end


module TACX_OF_CP =
struct
	module EVAL_VISITOR =
	struct
		type xnode = TACX.edge
		type xedge = TACX.edge
		type cons = xedge -> xedge -> xedge
		type extra = TACX.manager

		let do_leaf _ () = default_leaf
		let mu_cpx_of_cp = function
			| CpTypes.S -> Cpx2Types.S
			| CpTypes.P -> Cpx2Types.P
		let cpx_of_cp (b, u) = Cpx2Utils.spx_liste_to_block b false (List.map mu_cpx_of_cp u)
		let do_node extra = Extra.(CpGops.binload_tacx >> CpGops.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let edgeX = cpx_of_cp edgeX
			and edgeY = cpx_of_cp edgeY in
			let result = Utils.MNode Cpx2Gops.(fun nodeX nodeY ->
				TACX.push extra tag (Cpx2Utils.compose_edge edgeX nodeX) (Cpx2Utils.compose_edge edgeY nodeY)) in
			(result : (xnode, xnode -> xnode -> xnode) Utils.merge)))

		let do_edge _ e = Cpx2Utils.compose_edge (cpx_of_cp e)
	end

	module EVAL = Cp.TACX.NODE_VISITOR(EVAL_VISITOR)

	type manager = {
		man_cp : Cp.TACX.manager;
		mannni : TACX.manager;
		theman : EVAL.manager;
		calc   : Cp.TACX.edge -> TACX.edge
	}

	let newman tacx_cp tacx_nni =
		let theman, calc = EVAL.newman tacx_cp tacx_nni in
		{
			man_cp = tacx_cp;
			mannni = tacx_nni;
			theman = theman;
			calc   = calc;
		}, List.map calc
	
	let dump_stats man = Tree.Node [
		Tree.Node [Tree.Leaf "theman:"; EVAL.dump_stats man.theman];
	]


end

module CntSat =
struct
	module CntSat_VISITOR =
	struct
		type xnode = BigInt.big_int * BigInt.big_int
		type xedge = BigInt.big_int
		type extra = unit

		let cswap = Tools.cswap
		let add2 (x, y) (x', y') = BigInt.(x+x', y+y')
		let shift2 n (x, y) = BigInt.(shift_left x n, shift_left y n)
		let cnt_P liste = MyList.count (function Cpx2Types.P -> true | _ -> false) liste
		let cnt_S liste = MyList.count (function Cpx2Types.S -> true | _ -> false) liste

		let do_leaf () () = (BigInt.zero, BigInt.unit)

		let cntsat_block_spx arity (shift, tag, liste) cnt01 =
			shift2 (cnt_P liste) (match tag.Cpx2Types.maxX with
			| None -> cnt01
			| Some maxX ->
			(
				let cnt = Array.make (maxX+1) 0 in
				let clk x = cnt.(x) <- cnt.(x) + 1 in
				List.iter (function Cpx2Types.X (_, x) -> clk x | _ -> ()) liste;
				let rec aux ((if0, if1) as if01) under lvl =
					if lvl = -1 then if01
					else
					(
						assert(lvl>=0);
						let plus = match cnt.(lvl) with
							| cntx when cntx <= 0 -> assert false
							| 1 -> BigInt.(shift_left unit under)
							| cntx -> BigInt.(shift_left (shift_left unit cntx - unit) under)
						in
						let if01' = BigInt.(if Cpx2Utils.mod2 lvl
							then (if0, if1 + plus)
							else (if0 + plus, if1))
						in
						aux if01' (under+cnt.(lvl)) (lvl-1)
					)
				in
				let if01 = BigInt.(aux (zero, zero) 0 maxX) in
				add2 cnt01 (shift2 (cnt_S liste) (cswap shift if01))
			))
		
		let cntsat_block block cnt01 =
			let cnt01 = Cpx2Types.(match block.block with
				| C0 -> BigInt.(shift_left unit block.arity, zero)
				| Id _ -> (let half = BigInt.shift_left BigInt.unit (block.Cpx2Types.arity-1) in (half, half))
				| SPX(shift, tag, liste) -> cntsat_block_spx block.arity (shift, tag, liste) cnt01)
			in
			cswap block.Cpx2Types.neg cnt01

		let do_node ()    = Extra.(Cpx2DumpLoad.binload_node >> Cpx2Utils.block_split >> Cpx2Types.(fun (blockX, blockY) ->
			Utils.MNode (fun cnt01X cnt01Y -> add2 (cntsat_block blockX cnt01X) (cntsat_block blockY cnt01Y))))
		let do_edge () block cnt01 =
			let cnt0, cnt1 = cntsat_block block cnt01 in
			if block.Cpx2Types.neg then cnt1 else cnt0

	end

	module CntSat = GroBdd.NODE_VISITOR(CntSat_VISITOR)

	let newman man =
		CntSat.newman man ()
	
	let dump_stats = CntSat.dump_stats
	
end

(*

module AllSat =
(* BUG REPORTED *)
struct
	module AllSat_VISITOR =
	struct
		type xnode = (bool option list list) * (bool option list list)
		type xedge = (bool option list list)
		type extra = unit

		let cswap b (x, y) = if b then (y, x) else (x, y)
		let x block =
			let _, maxX = Cpx2Utils.classify block in
			match maxX with
			| None -> ([], [])
			| Some maxX ->
				let llist = MyList.init (maxX+1) Cpx2Types.(fun i ->
					let rec aux0 chain = function
						| [] -> List.rev chain
						| head::tail -> match head with
							| P -> aux0 (None::chain) tail
							| S -> aux0 (None::chain) tail
							| X(b, j) -> if j < i
								then aux0 ((Some(not b))::chain) tail
								else aux0 (None::chain) tail
					in
					let rec aux carry chain = function
					| [] -> List.rev carry
					| head::tail -> match head with
						| P -> aux carry (None::chain) tail
						| S -> aux carry (None::chain) tail
						| X(b, j) ->
							if i = j
							then aux ((aux0 ((Some b)::chain) tail)::carry) ((Some(not b))::chain) tail
							else if i < j
							then aux carry (None::chain) tail
							else aux carry ((Some(not b))::chain) tail
					in aux [] [] block.sub) in
				let rec aux (if0, if1) = function
					| [] -> (if0, if1)
					| [x] -> (if0@x, if1)
					| x::y::tail -> aux (if0@x, if1@y) tail
				in
				let if01 = aux ([], []) llist in
				Tools.cswap block.Cpx2Types.shift if01

		let compose =
			let rec aux carry = Cpx2Types.(function
			  | ([], [])			 -> List.rev carry
			  | (S      ::x', y::y') -> aux (y            ::carry) (x', y')
			  | (P      ::x', y'   ) -> aux (None		  ::carry) (x', y')
			  | (X(b, _)::x', y'   ) -> aux ((Some(not b))::carry) (x', y')
			  | _ -> assert false)
			in fun block elem -> aux [] (block.Cpx2Types.sub, elem)

		let map_compose block (liste0, liste1) = (List.map (compose block) liste0, List.map (compose block) liste1)

		let map_push b (liste0, liste1) = (List.map (fun x -> (Some b)::x) liste0, List.map (fun x -> (Some b)::x) liste1)

		let add (x, y) (x', y') = (x@x', y@y')

		let shift block l01 = Cpx2Types.(cswap block.neg (add (x block) (map_compose block l01)))

		let do_leaf (():extra) (():GroBdd.M.leaf) = (([], [[]]):xnode)
		let do_node ()    = Extra.(Cpx2DumpLoad.binload_node >> Cpx2Utils.block_split >> Cpx2Types.(fun (blockX, blockY) ->
			Utils.MNode (fun lX lY -> add
				(map_push false (shift blockX lX))
				(map_push true  (shift blockY lY)))))
		let do_edge (():extra) (block:GroBdd.M.edge) (l01:xnode) = shift block l01 |> snd
	end

	module AllSat = GroBdd.NODE_VISITOR(AllSat_VISITOR)

	let newman man =
		AllSat.newman man ()
	
	let dump_stats = AllSat.dump_stats
	
end

*)

module PartEval =
struct
	module Eval_VISITOR : GroBdd.MODELE_EVAL with
			type pars = bool option list option
		and type back = GroBdd.M.edge
	=
	struct
		type pars = bool option list option
		type back = GroBdd.M.edge

		let pars gid pars (ex, ix) =
			let (ex, ix), pars = Cpx2Utils.assign_edge pars (ex, ix) in
			match pars with
			| None -> Utils.MStop (ex, ix)
			| Some [] -> assert false
			| Some (head::tail) -> match head with
				| None			-> Utils.MPull	(ex, (Some tail), (Some tail))
				| Some false	-> Utils.Go0	(ex, (Some tail))
				| Some true		-> Utils.Go1	(ex, (Some tail))

		let back = Cpx2Utils.compose_edge
	end

	include GroBdd.EVAL(Eval_VISITOR)

end

module EVALE =
struct
	module EVAL_VISITOR =
	struct
		type xnode = GroBdd.edge
		type xedge = GroBdd.edge
		type cons = xedge -> xedge -> xedge
		type extra = cons * cons * cons (* (a, c, x) *)

		let do_leaf _ () = default_leaf
		let do_node (a, c, x) = Extra.(Cpx2DumpLoad.binload_tacx >> Cpx2Utils.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let merge = TacxTypes.(match tag with And -> a | Cons -> c | Xor -> x) in
			Utils.MNode (fun nodeX nodeY -> merge (Cpx2Utils.compose_edge edgeX nodeX) (Cpx2Utils.compose_edge edgeY nodeY))))
		let do_edge _ = Cpx2Utils.compose_edge
	end

	module EVAL = TACX.NODE_VISITOR(EVAL_VISITOR)

	type manager = {
		grobdd : GroBdd.manager;
		andman : AND.manager;
		xorman : XOR.manager;
		theman : EVAL.manager;
		calc   : TACX.edge -> GroBdd.edge
	}

	let newman tacx man =
		let c = GroBdd.push man in
		let aman, a = AND.newman man
		and xman, x = XOR.newman man in
		let theman, calc = EVAL.newman tacx (a, c, x) in
		{
			grobdd = man;
			andman = aman;
			xorman = xman;
			theman = theman;
			calc = calc
		}, List.map calc
	
	let dump_stats man = Tree.Node [
(*		Tree.Node [Tree.Leaf "grobdd:"; GroBdd.dump_stats man.grobdd]; *)
		Tree.Node [Tree.Leaf "andman:"; AND.dump_stats man.andman];
		Tree.Node [Tree.Leaf "xorman:"; XOR.dump_stats man.xorman];
		Tree.Node [Tree.Leaf "theman:"; EVAL.dump_stats man.theman];
	]


end

module TacxPartEval =
struct
	module Eval_VISITOR : TACX.MODELE_IUOP with
			type eval = bool option list
	=
	struct
		type eval = bool option list

		let eval pars (ex, ix) =
			let (ex, ix), pars = Cpx2Utils.assign_edge' pars (ex, ix) in
			match pars with
			| None -> Utils.MEdge(ex, ix)
			| Some pars -> match ix with
				| Utils.Leaf () -> assert false
				| Utils.Node node -> Utils.MNode(ex, (pars, node))
		let read mess = function
			| TacxTypes.And -> Utils.MPull (TacxTypes.And, mess, mess)
			| TacxTypes.Xor -> Utils.MPull (TacxTypes.Xor, mess, mess)
			| TacxTypes.Cons -> match mess with
				| [] -> assert false
				| head::tail -> match head with
					| None -> Utils.MPull (TacxTypes.Cons, mess, mess)
					| Some false -> Utils.Go0 mess
					| Some true  -> Utils.Go1 mess
	end

	include TACX.IUOP(Eval_VISITOR)

end

(*

module PURE_TO_BRYANT =
struct
	module CONS_VISITOR =
	struct
		type xedge = Bryant.GroBdd.edge
		type xresi = bool * int
		type extra = Bryant.GroBdd.manager

		let do_edge _ (block, i) = match Cpx2Utils.block_is_const block with
			| Some b ->
			(
				Utils.MEdge ((b, (Cpx2Utils.block_size block, 0)), Utils.Leaf())
			)
			| None ->
			Cpx2Types.(
				let rec aux carry = function
					| [] -> assert false
					| head::tail as liste -> match head with
						| P -> aux (1+carry) tail
						| _			-> carry, liste
				in 
				let shift, sub' = aux 0 block.sub in
				let block' = {neg = false; shift = block.shift; sub = sub'} in
				Utils.MNode ((block.neg, shift), (block', i))
			)

		let push = Bryant.GroBdd.push

		let compose _ ((neg, shift):xresi) ((b, (x, y)), i) = ((neg<>b, (shift+x, y)), i)
	end

	include GroBdd.CONS_VISITOR(CONS_VISITOR)

end

*)

module GetSize =
struct
	module MODELE_VISITOR =
	struct
		type xnode = unit
		type xedge = unit
		type extra = int ref

		let do_leaf extra (():GroBdd.M.leaf) = ()
		let do_node extra (c:GroBdd.M.node) = extra := !extra + Bitv.length c; Utils.MNode (fun () () -> ())
		let do_edge extra _ () = ()
	end

	module VISITOR = GroBdd.NODE_VISITOR(MODELE_VISITOR)

	let newman man =
		VISITOR.newman man (ref 0)
	
	let dump_stats = VISITOR.dump_stats

	let get man = !(VISITOR.extra man)
	
end
