(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open Extra
open O3Extra

let o3s_leaf = BinO3.unit
let o3s_next' o3s_ident = Utils.o3s_gnode o3s_leaf o3s_ident

module GroBdd =
struct
	module M0 =
	struct
		type leaf = unit
		type edge = Cpx2BTypes.block
		type node = unit
		
		let strdump_leaf = StrDump.unit
		let strdump_edge = Cpx2BDump.block
		let strdump_node = StrDump.unit
	
		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = Cpx2BO3.o3s_edge
		let o3s_node = BinO3.unit

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node Cpx2BO3.o3s_node o3s_leaf o3s_ident

		let __check_reverse__ = false

	end

	module M1 =
	struct
		module M = M0

		let arity     = Cpx2BUtils.arity_edge
		let compose   = Cpx2BUtils.compose_edge
		let push_node = Cpx2BGops.solve_cons
		let pull_node = Cpx2BGops.node_pull
	end

	include BinUbdagTC.STDIO(M1)

	module TO_DOT_MODELE =
	struct
		module M = G0

		let string_of_leaf () = "L0"

		let string_of_pos = function
			| None       -> "black"
			| Some false -> "red"
			| Some true  -> "green"

		let string_of_edge pos edge =
			"[label = \""^(Cpx2BO3.pretty_block edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node _ = ""
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile = TO_DOT.dotfile

	module PEVAL_MODELE =
	struct
		module M = G1

		type peval = GUtils.peval

		let o3s_peval = BinO3.bool_option_list

		type next = peval option * M.G.ident

		type next' = next M.M.M.next'
		type edge' = next M.M.M.edge'
		type node' = next M.M.M.node'
		
		let eval_edge : peval -> edge' -> edge' = Cpx2BUtils.peval_pedge
		let eval_node : peval -> node' -> (edge', node') Utils.merge = Cpx2BUtils.peval_pnodeC
	end

	module PEVAL = BinUbdagTC.PEVAL_CACHED(PEVAL_MODELE)

	module AND = BinUbdagTC.EBINOP_CACHED
		(struct module M = PEVAL let solver = Cpx2BGops.solve_and end)

	module XOR = BinUbdagTC.EBINOP_CACHED
		(struct module M = PEVAL let solver = Cpx2BGops.solve_xor end)


	type manager = {
		cman : G1.manager;
		eman : PEVAL.manager;
		aman : AND.manager;
		xman : XOR.manager;
		cmap :                        G0.node' -> G0.edge';
		emap :       PEVAL.M.peval -> G0.edge' -> G0.edge';
		amap :                        G0.node' -> G0.edge';
		xmap :                        G0.node' -> G0.edge';
		solve_tacx : TacxTypes.tag -> G0.node' -> G0.edge';
	}

	let get_cman man = man.cman
	let get_eman man = man.eman
	let get_aman man = man.aman
	let get_xman man = man.xman

	let get_cmap man = man.cmap
	let get_emap man = man.emap
	let get_amap man = man.amap
	let get_xmap man = man.xmap


	let dump_stats man = Tree.Node [
		Tree.Node [Tree.Leaf "cons"; G1.dump_stats man.cman];
		Tree.Node [Tree.Leaf "eva "; PEVAL.dump_stats man.eman];
		Tree.Node [Tree.Leaf "and "; AND.dump_stats man.aman];
		Tree.Node [Tree.Leaf "xor "; XOR.dump_stats man.xman];
	]

	let push man = man.cmap

	let makeman hsize =
		let cman = G1.makeman hsize in
		let eman = PEVAL.makeman cman hsize in
		let aman = AND.makeman cman eman hsize
		and xman = XOR.makeman cman eman hsize in
		let cmap = G1.push cman
		and emap = PEVAL.map_edge eman
		and amap  = AND.map aman
		and xmap  = XOR.map xman in
		let solve_tacx = TacxTypes.(function
			| Cons -> cmap
			| And  -> amap
			| Xor  -> xmap
		) in
		{cman; eman; aman; xman; cmap; emap; amap; xmap; solve_tacx}

    let default_newman_hsize = 10000

	let newman () = makeman default_newman_hsize


	module CntSat_MODELE =
	struct
		module M = G0

		type xnode  = BigInt.big_int * BigInt.big_int
		type xedge  = BigInt.big_int
		type extra  = unit

		let cswap = Tools.cswap
		let add2 (x, y) (x', y') = BigInt.(x+x', y+y')
		let shift2 n (x, y) = BigInt.(shift_left x n, shift_left y n)
		let cnt_P liste = MyList.count (function Cpx2BTypes.P -> true | _ -> false) liste
		let cnt_S liste = MyList.count (function Cpx2BTypes.S -> true | _ -> false) liste


		let cntsat_block_spx arity (shift, tag, liste) cnt01 =
			shift2 (cnt_P liste) (match tag.Cpx2BTypes.maxX with
			| None -> cnt01
			| Some maxX ->
			(
				let cnt = Array.make (maxX+1) 0 in
				let clk x = cnt.(x) <- cnt.(x) + 1 in
				List.iter (function Cpx2BTypes.X (_, x) -> clk x | _ -> ()) liste;
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
						let if01' = BigInt.(if Cpx2BUtils.mod2 lvl
							then (if0, if1 + plus)
							else (if0 + plus, if1))
						in
						aux if01' (under+cnt.(lvl)) (lvl-1)
					)
				in
				let if01 = BigInt.(aux (zero, zero) 0 maxX) in
				add2 cnt01 (shift2 (cnt_S liste) (cswap (not shift) if01))
			))
		
		let cntsat_block block cnt01 =
			let cnt01 = Cpx2BTypes.(match block.block with
				| C0 -> BigInt.(zero, shift_left unit block.arity)
				| Id _ -> (let half = BigInt.shift_left BigInt.unit (block.Cpx2BTypes.arity-1) in (half, half))
				| SPX(shift, tag, liste) -> cntsat_block_spx block.arity (shift, tag, liste) cnt01)
			in
			cswap block.Cpx2BTypes.neg cnt01

		type next' = (unit -> xnode) M.M.next'
		type edge' = (unit -> xnode) M.M.edge'
		type node' = (unit -> xnode) M.M.node'
		
		let rec_next = function
			| Utils.Leaf () -> BigInt.(zero, unit)
			| Utils.Node node -> node()

		let rec_edge (block, next) =
			cntsat_block block (rec_next next)

		let map_node () ((), edge0, edge1) =
			add2 (rec_edge edge0) (rec_edge edge1)

		let map_edge () edge = rec_edge edge |> fst
	end

	module CntSat = BinUbdag.EXPORT_NOC(CntSat_MODELE)

	let cntsat grobdd edges =
		let man = CntSat.newman (G1.export grobdd) () in
		let map = CntSat.rec_edge man in
		(man, List.map map edges)
(*
	module AllSat_MODELE =
	struct
		module M = G0

		type xnode  = (bool option list list) * (bool option list list)
		type xnode' = xnode
		type xedge  = (bool option list list)
		type extra  = unit

		let dump_xnode x = x
		let load_xnode x = x

		type next' = (unit -> xnode) M.M.next'
		type edge' = (unit -> xnode) M.M.edge'
		type node' = (unit -> xnode) M.M.node'

		let cswap b (x, y) = if b then (y, x) else (x, y)
		let p = function Cpx2BTypes.P -> true | _ -> false
		let compose =
			let rec aux carry = function
			  | ([], []) -> List.rev carry
			  | (Cpx2BTypes.S::x', y::y') -> aux (y::carry) (x', y')
			  | (Cpx2BTypes.P::x', y') -> aux (None::carry) (x', y')
			  | _ -> assert false
			in function
			  | None	-> fun deco elem -> aux [] (deco, elem)
			  | Some b	-> fun deco elem -> aux [Some b] (deco, elem)

		let rec_next : next' -> xnode = function
			| Utils.Leaf () -> ([], [[]])
			| Utils.Node node -> node()

		let rec_edge side ((neg, sub), next) : xnode =
			let if0, if1 = rec_next next in
			let if0 = List.map (compose side sub) if0
			and if1 = List.map (compose side sub) if1 in
			cswap neg (if0, if1)

		let add (x0, y0) (x1, y1) = (x0@x1, y0@y1)
		
		let map_node () ((), edge0, edge1) : xnode=
			add (rec_edge (Some false) edge0) (rec_edge (Some true) edge1)

		let map_edge () edge : xedge = rec_edge None edge |> fst
	end

	module AllSat = BinUbdag.EXPORT(AllSat_MODELE)
*)

	module DummyCntSat_MODELE =
	struct
		module M = G1

		type xedge = BigInt.big_int * BigInt.big_int
		type extra = unit

		let cswap = Tools.cswap
		let add2 (x, y) (x', y') = BigInt.(x+x', y+y')

		let map_edge () apply ((block, next) as edge) = match Cpx2BUtils.edge_is_const edge with
			| Some neg -> cswap neg BigInt.(zero, shift_left unit block.Cpx2BTypes.arity)
			| None -> apply edge

		let push () ((), x, y) = add2 x y
	end

	module DummyCntSat = BinUbdagTC.EXPORT_NOC(DummyCntSat_MODELE)

	let cntsat' (grobdd : G1.manager) edges =
		let man = DummyCntSat.newman grobdd () in
		let map = DummyCntSat.map_edge man >> fst in
		(man, List.map map edges)

	module TO_BRYANT_MODELE =
	struct
		module M = G1

		type xedge  = BryantB.GroBdd.G0.edge'
		type xedge' = Bitv.t
		type extra  = BryantB.GroBdd.G1.manager

		let o3_xedge = BryantB.GroBdd.G0.o3b_edge'

		let map_edge _ apply ((block, next) as edge) = match Cpx2BUtils.edge_is_const edge with
			| Some neg -> ((neg, (block.Cpx2BTypes.arity, 0)), Utils.Leaf())
			| None     -> (apply edge)

		let push bryant = BryantB.GroBdd.G1.push bryant
	end

	module TO_BRYANT = BinUbdagTC.EXPORT(TO_BRYANT_MODELE)

	module TO_ZDD_MODELE =
	struct
		module M = G1

		type xedge  = ZddB.GroBdd.G0.edge'
		type xedge' = Bitv.t
		type extra  = ZddB.GroBdd.G1.manager

		let o3_xedge = ZddB.GroBdd.G0.o3b_edge'

		let map_edge _ apply ((block, next) as edge) = match Cpx2BUtils.edge_is_const edge with
			| Some neg -> ((block.Cpx2BTypes.arity, 0), Utils.Leaf neg)
			| None     -> (apply edge)

		let push bryant = ZddB.GroBdd.G1.push bryant
	end

	module TO_ZDD = BinUbdagTC.EXPORT(TO_ZDD_MODELE)

	module OF_CP_MODELE =
	struct
		module M = CpB.GroBdd.G0

		type extra  = G1.manager
		type xnode  = G0.edge'
		type xnode' = Bitv.t
		type xedge  = G0.edge'

		let o3_xnode = G0.o3b_edge'

		let map_edge (neg, u) next_is_leaf=
			let funmap = function
				| CpTypes.S -> Cpx2BTypes.S
				| CpTypes.P -> Cpx2BTypes.P
			in
			Cpx2BUtils.reduce_block_spx neg (List.length u) next_is_leaf false (List.map funmap u)

		let rec_edge (edge, next) =
			let edge = map_edge edge (next = Utils.Leaf())in
			let edge = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node next -> M1.compose edge (next()) in
			assert(Cpx2BUtils.check_edge edge);
			edge

		let map_node man (node, edge0, edge1) = G1.push man (node, rec_edge edge0, rec_edge edge1)
		let map_edge _ = rec_edge
	end

	module OF_CP = BinUbdag.EXPORT(OF_CP_MODELE)
	
	let of_cp pure_cp edges =
		let pure_cpx = G1.newman () in
		let man = OF_CP.newman (CpB.GroBdd.G1.export pure_cp) pure_cpx in
		let map = OF_CP.rec_edge man in
		(man, (pure_cpx, List.map map edges))

	module QUANT_MODELE =
	struct
		module M = G1

		type peval = bool list
		
		let o3s_peval = BinO3.(list bool)

		type next = peval option * M.G.ident

		type next' = next M.M.M.next'
		type edge' = next M.M.M.edge'
		type node' = next M.M.M.node'

		let eval_edge = Cpx2BGops.quant_qedge
		let eval_node = Cpx2BGops.solve_quant
	end

	module QUANT = BinUbdagTC.QUANT(QUANT_MODELE)
end

module TACX =
struct

	module M0 =
	struct
		type leaf = unit
		type edge = Cpx2BTypes.block
		type node = TacxTypes.tag

		let strdump_leaf = StrDump.unit
		let strdump_edge = Cpx2BDump.block
		let strdump_node = TacxTypes.strdump_tag

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = Cpx2BO3.o3s_edge
		let o3s_node = TacxTypes.o3s_tag

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node Cpx2BO3.o3s_tacx o3s_leaf o3s_ident

		let __check_reverse__ = false

	end

	module M1 =
	struct
		module M = M0

		open Cpx2BUtils

		type peval = bool option list

		let o3s_peval = BinO3.bool_option_list

		let eval_edge = Cpx2BUtils.peval_pedge

		let eval_node peval node = TacxTypes.default_eval_node eval_edge peval node

		type 'i link = peval option * 'i
		type 'i next'' = 'i link M.next'
		type 'i edge'' = 'i link M.edge'
		type 'i node'' = 'i link M.node'
		type 'i tree'' = ('i next'', M.edge, M.node) GTree.edge

		let o3s_link   o3s_ident = (BinO3.option o3s_peval) +* o3s_ident
		let o3s_next'' o3s_ident = Utils.o3s_gnode o3s_leaf (o3s_link o3s_ident)
		let o3s_edge'' o3s_ident = M.o3s_edge +* (o3s_next'' o3s_ident)
		let o3s_node'' o3s_ident =
			BinUbdag.default_o3s_node Cpx2BO3.o3s_tacx o3s_leaf (o3s_link o3s_ident)
		let o3s_tree'' o3s_ident =
			GTree.o3s_edge (o3s_next'' o3s_ident) M.o3s_edge M.o3s_node

		let map_edge (edge, next) = (edge, GTree.Leaf next)

		let map_node (node, edge0, edge1) =
			GTree.Node(node, map_edge (reduce_edge edge0), map_edge (reduce_edge edge1))

		let push_node node = 
(*
			let blockC = Cpx2BUtils.(make_block_S false (arity_node node)) in
			(blockC, map_node node)
*)
			let edge, merge = Cpx2BGops.solve_tacx node in
			(edge, match merge with
			| Utils.MEdge next -> GTree.Leaf next
			| Utils.MNode node ->   map_node node)

		let compose = Cpx2BUtils.compose_edge
	end

	include BinUbdagTE.STDIO(M1)

	module TO_DOT_MODELE =
	struct
		module M = G0

		let string_of_leaf () = "L0"

		let string_of_pos = function
			| None       -> "black"
			| Some false -> "red"
			| Some true  -> "green"

		let string_of_edge pos edge =
			"[label = \""^(Cpx2BO3.pretty_block edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node node = "[label = \""^(TacxTypes.strdump_tag node)^"\"];"
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile ubdag edges target =
		TO_DOT.dotfile (G1.export ubdag) edges target

	module COMPILE_MODELE =
	struct
		module M = G0
	
		type extra  = GroBdd.manager
		type xnode  = GroBdd.G0.edge'
		type xnode' = Bitv.t
		type xedge  = GroBdd.G0.edge'

		let o3_xnode = GroBdd.G0.o3b_edge'

		let rec_edge (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node node -> GroBdd.M1.compose edge (node())

		let map_node man (tag, edge0, edge1) =
			man.GroBdd.solve_tacx tag ((), rec_edge edge0, rec_edge edge1)
		let map_edge man edge = rec_edge edge
	end

	module COMPILE = BinUbdag.EXPORT(COMPILE_MODELE)

	let compile tacx edges =
		let grobdd = GroBdd.newman () in
		let man = COMPILE.newman (G1.export tacx) grobdd in
		let map = COMPILE.rec_edge man in
		(man, (grobdd, List.map map edges))

	type manager = {
		grobdd : GroBdd.manager;
		tacx : G1.manager;
		push : G0.node' -> G0.edge';
		man : COMPILE.manager;
		map : G0.edge' -> GroBdd.G0.edge';
	}

	let makeman grobdd hsize =
		let tacx = G1.makeman hsize in
		let push = G1.push tacx in
		let man = COMPILE.makeman (G1.export tacx) grobdd hsize in
		let map = COMPILE.rec_edge man in
		{grobdd; tacx; push; man; map}

    let default_newman_hsize = 10000

	let newman grobdd = makeman grobdd default_newman_hsize

	module OF_CP_MODELE =
	struct
		module M = CpB.TACX.G0

		type extra  = G1.manager
		type xnode  = G0.edge'
		type xnode' = Bitv.t
		type xedge  = G0.edge'

		let o3_xnode = G0.o3b_edge'

		let map_edge (neg, u) =
			let funmap = function
				| CpTypes.S -> Cpx2BTypes.S
				| CpTypes.P -> Cpx2BTypes.P
			in
			Cpx2BUtils.reduce_block_spx neg (List.length u) false false (List.map funmap u)

		let rec_edge (edge, next) =
			let edge = map_edge edge in
			match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node next -> M1.compose edge (next())

		let map_node man (node, edge0, edge1) = G1.push man (node, rec_edge edge0, rec_edge edge1)
		let map_edge _ = rec_edge
	end

	module OF_CP = BinUbdag.EXPORT(OF_CP_MODELE)

	let of_cp pure_cp edges =
		let pure_cpx = G1.newman () in
		let man = OF_CP.newman (CpB.TACX.G1.export pure_cp) pure_cpx in
		let map = OF_CP.rec_edge man in
		(man, (pure_cpx, List.map map edges))

	module CHECK_MODELE =
	struct
		module M = G0

		type extra = unit
		type xnode = int
		type xedge = unit

		let rec_next = function
			| Utils.Leaf () -> 0
			| Utils.Node node -> node()

		let rec_edge ((edge, next) as edge') =
			let arity_next = rec_next next in
			let arity_in   = Cpx2BUtils.count_nS_block edge  in
			if not(Cpx2BUtils.check_edge edge' && arity_next = arity_in)
			then
			(
				print_newline();
				print_string (M0.strdump_edge edge);
				print_newline();
				print_string (Utils.(function Leaf () -> "Leaf" | Node _ -> "Node") next);
				print_string "arity next: ";
				print_int arity_next;
				print_newline();
				raise (Utils.StrError "TACX - check failure")
			);
			Cpx2BUtils.arity_block edge

		let map_node () (tag, edge0, edge1) =
			let n0 = rec_edge edge0
			and n1 = rec_edge edge1 in
			assert(n0 = n1);
			TacxTypes.(match tag with
			| Cons -> n0+1
			| And
			| Xor  -> n0)

		let map_edge () edge = ignore(rec_edge edge)
	end

	module CHECK = BinUbdag.EXPORT_NOC(CHECK_MODELE)

	let check tacx edges =
		let man = CHECK.newman (G1.export tacx) () in
		let map = CHECK.rec_edge man in
		try (List.iter map edges; true)
		with Utils.StrError "TACX - check failure" -> false

	
	module FINITE_SOLVER_MODELE =
	struct
		module M = G1

		type leaf = M.M.M.leaf
		type edge = M.M.M.edge
		type node = M.M.M.node

		type ident = M.G.ident
		type next' = M.G.next'
		type edge' = M.G.edge'
		type node' = M.G.node'

		type next'' = M.next''
		type edge'' = M.edge''
		type node'' = M.node''
		type tree'' = M.tree''

		let arity_limit = 20

		let rewrite ((tag, edgeX, edgeY) as node) =
			let arity = Cpx2BUtils.arity_edge edgeX in
			Cpx2BGops.rewrite_expand_AX_node (arity <= arity_limit) node
	end

	module FINITE_SOLVER = BinUbdagTE.REWRITE(FINITE_SOLVER_MODELE)

	let finite_solver man edges =
		let man = FINITE_SOLVER.newman man in
		let map = man.FINITE_SOLVER.map None in
		(man, (List.map map edges))
		


end

let newman = GroBdd.newman

let make_const b n = ((Cpx2BUtils.make_block_C0 b n), Utils.Leaf())

let make_ident man b n = GroBdd.push man ((), (make_const b n), (make_const (not b) n))

let arity block = block.Cpx2BTypes.arity

let push_pass = Cpx2BUtils.push_P_block

let no = Cpx2BUtils.neg

let is_root = Cpx2BUtils.edge_is_const

let get_root = Cpx2BUtils.get_root

let ( *! ) man x y = man.TACX.push (TacxTypes.Cons, x, y)
let ( &! ) man x y = man.TACX.push (TacxTypes.And , x, y)
and ( ^! ) man x y = man.TACX.push (TacxTypes.Xor , x, y)

(*
module QTACX =
struct

	module M0 =
	struct
		type leaf = unit
		type edge = Cpx2BTypes.block Qte.qte
		type node = TacxTypes.tag

		let strdump_leaf = StrDump.unit
		let strdump_edge = QteO3.strdump_qte Cpx2BDump.block
		let strdump_node = TacxTypes.strdump_tag

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = QteO3.o3s_qte Cpx2BO3.o3s_edge
		let o3s_node = TacxTypes.o3s_tag

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node (QteO3.default_o3s_node Cpx2BO3.o3s_edge) o3s_leaf o3s_ident

		let __check_reverse__ = false

	end

	module M1 =
	struct
		module M = M0

		let push_node = Qte.solve_node

		let compose = Qte.compose

	end

	include BinUbdagT.STDIO(M1)

	module TO_DOT_MODELE =
	struct
		module M = G0

		let string_of_leaf () = "L0"

		let string_of_pos = function
			| None       -> "black"
			| Some false -> "red"
			| Some true  -> "green"

		let string_of_edge pos edge =
			"[label = \""^(Cpx2BO3.pretty_block edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node = TacxTypes.strdump_tag
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile ubdag edges target =
		TO_DOT.dotfile (G1.export ubdag) edges target

	module COMPILE_MODELE =
	struct
		module M = G0
	
		type extra  = GroBdd.manager
		type xnode  = GroBdd.G0.edge'
		type xnode' = Bitv.t
		type xedge  = GroBdd.G0.edge'

		let o3_xnode = GroBdd.G0.o3b_edge'

		let rec_edge (edge, next) = match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node node -> GroBdd.M1.compose edge (node())

		let map_node man (tag, edge0, edge1) =
			man.GroBdd.solve_tacx tag ((), rec_edge edge0, rec_edge edge1)
		let map_edge man edge = rec_edge edge
	end

	module COMPILE = BinUbdag.EXPORT(COMPILE_MODELE)

	let compile tacx edges =
		let grobdd = GroBdd.newman () in
		let man = COMPILE.newman (G1.export tacx) grobdd in
		let map = COMPILE.rec_edge man in
		(man, (grobdd, List.map map edges))

	type manager = {
		grobdd : GroBdd.manager;
		tacx : G1.manager;
		push : G0.node' -> G0.edge';
		man : COMPILE.manager;
		map : G0.edge' -> GroBdd.G0.edge';
	}

	let makeman grobdd hsize =
		let tacx = G1.makeman hsize in
		let push = G1.push tacx in
		let man = COMPILE.makeman (G1.export tacx) grobdd hsize in
		let map = COMPILE.rec_edge man in
		{grobdd; tacx; push; man; map}

    let default_newman_hsize = 10000

	let newman grobdd = makeman grobdd default_newman_hsize

	module OF_CP_MODELE =
	struct
		module M = CpB.TACX.G0

		type extra  = G1.manager
		type xnode  = G0.edge'
		type xnode' = Bitv.t
		type xedge  = G0.edge'

		let o3_xnode = G0.o3b_edge'

		let map_edge (neg, u) =
			let funmap = function
				| CpTypes.S -> Cpx2BTypes.S
				| CpTypes.P -> Cpx2BTypes.P
			in
			Cpx2BUtils.reduce_block_spx neg (List.length u) false false (List.map funmap u)

		let rec_edge (edge, next) =
			let edge = map_edge edge in
			match next with
			| Utils.Leaf leaf -> (edge, Utils.Leaf leaf)
			| Utils.Node next -> M1.compose edge (next())

		let map_node man (node, edge0, edge1) = G1.push man (node, rec_edge edge0, rec_edge edge1)
		let map_edge _ = rec_edge
	end

	module OF_CP = BinUbdag.EXPORT(OF_CP_MODELE)

	let of_cp pure_cp edges =
		let pure_cpx = G1.newman () in
		let man = OF_CP.newman (CpB.TACX.G1.export pure_cp) pure_cpx in
		let map = OF_CP.rec_edge man in
		(man, (pure_cpx, List.map map edges))

	module CHECK_MODELE =
	struct
		module M = G0

		type extra = unit
		type xnode = int
		type xedge = unit

		let rec_next = function
			| Utils.Leaf () -> 0
			| Utils.Node node -> node()

		let rec_edge ((edge, next) as edge') =
			let arity_next = rec_next next in
			let arity_in   = Cpx2BUtils.count_nS_block edge  in
			if not(Cpx2BUtils.check_edge edge' && arity_next = arity_in)
			then
			(
				print_newline();
				print_string (M0.strdump_edge edge);
				print_newline();
				print_string (Utils.(function Leaf () -> "Leaf" | Node _ -> "Node") next);
				print_string "arity next: ";
				print_int arity_next;
				print_newline();
				raise (Utils.StrError "TACX - check failure")
			);
			Cpx2BUtils.arity_block edge

		let map_node () (tag, edge0, edge1) =
			let n0 = rec_edge edge0
			and n1 = rec_edge edge1 in
			assert(n0 = n1);
			TacxTypes.(match tag with
			| Cons -> n0+1
			| And
			| Xor  -> n0)

		let map_edge () edge = ignore(rec_edge edge)
	end

	module CHECK = BinUbdag.EXPORT_NOC(CHECK_MODELE)

	let check tacx edges =
		let man = CHECK.newman (G1.export tacx) () in
		let map = CHECK.rec_edge man in
		try (List.iter map edges; true)
		with Utils.StrError "TACX - check failure" -> false

end
*)
