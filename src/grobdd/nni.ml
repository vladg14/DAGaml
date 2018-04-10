(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

let strdump_node = STD.bitv_hexa
let strload_node = Extra.(STL.bitv_hexa >> NniGops.binload_node >> NniGops.node_split)

let strdump_tacx = STD.bitv_hexa
let strload_tacx = Extra.(STL.bitv_hexa >> NniGops.binload_tacx >> NniGops.tacx_split)

let strdump_edge = Extra.(NniGops.bindump_edge >> STD.bitv_hexa)
let strload_edge = Extra.(STL.bitv_hexa >> NniGops.binload_edge)

let dot_of_edge_aux color edge =
	"[label = \""^(NniGops.strdump_edge edge)^"\"; color=\""^color^"\"];"

let dot_of_edge = dot_of_edge_aux "black"

let dot_of_node node =
	let e0, e1 = NniGops.binload_node node |> NniGops.node_split in
	"", (dot_of_edge_aux "red" e0), (dot_of_edge_aux "blue" e1)

let default_leaf = ((false, []), Utils.Leaf ())

let strdump_leaf = (fun () -> Tree.Node [])
let strload_leaf = (function Tree.Node [] -> default_leaf | _ -> assert false)

module GroBdd_M : Subdag.MODELE with
		type node = Bitv.t
	and	type edge = NniTypes.edge_state
	and type leaf = unit
=
struct
	
	type node = Bitv.t
	type edge = NniTypes.edge_state
	type leaf = unit

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push : ('t -> 'i) -> 't e -> 't e -> ('t e, edge * 't n) Utils.merge =
		NniGops.node_push_cons
	let pull = NniGops.node_pull
	let compose = NniGops.compose
	
	let pull_node = NniGops.node_pull_node
	
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

	let push man edgeX edgeY =
		let edgeXY = push man edgeX edgeY in
		let edgeX', edgeY' = pull man edgeXY in
		assert ((edgeX = edgeX')&&(edgeY = edgeY'));
		edgeXY

	let dumpfile man edges target =
		let strman = Udag.STree.newman() in
		let stredges = dump man strman edges in
		Udag.STree.dumpfile strman stredges target;
		strman
	
	let loadfile target =
		let strman, stredges = Udag.STree.loadfile target in
		let man = newman () in
		let edges = load man strman stredges in
		man, edges

	let stree_dump man edges =
		let strman = Udag.STree.newman() in
		let stredges = dump man strman edges in
		Udag.STree.dump strman stredges

	let stree_load stree =
		let strman, stredges = Udag.STree.load stree in
		let man = newman () in
		let edges = load man strman stredges in
		man, edges
end

let newman = GroBdd.newman

let make_const b n = GroBdd.push_leaf (b, MyList.ntimes NniTypes.(P(false, None)) n) ();;

let make_ident man b n = GroBdd.push man (make_const b n) (make_const (not b) n);;

let arity ((_, l), _) = List.length l

let push_pass ((b, l), i) = ((b, NniTypes.(P(false, None))::l), i)

let no ((b, l), i) = ((not b, l), i)

let is_root = function
	| ((b, l), Utils.Leaf ()) -> Some b
	| _ -> None 

let get_root b ((_, l), _) = ((b, MyList.ntimes NniTypes.(P(false, None)) (List.length l)), Utils.Leaf ())

let (=??) (ex, ix) (ey, iy) = match ix, iy with
	| Utils.Leaf (), Utils.Leaf () -> ex = ey
	| Utils.Node nx, Utils.Node ny -> (nx == ny) && (ex = ey)
	| _ -> false

module AND_M : GroBdd.MODELE_IBOP =
struct
	type t = Bitv.t
	let compare = Pervasives.compare
	type transform = NniTypes.edge_state
	let compose = NniGops.compose
	let decomp x y c =
		let ((bx, by), lxy) = NniGops.binload_node_and c in
		let lx, ly = NniGops.pair_split lxy in
		((bx, lx), x), ((by, ly), y)
	let solver = NniGops.node_solve_and
end;;

module XOR_M : GroBdd.MODELE_IBOP =
struct
	type t = Bitv.t
	let compare = Pervasives.compare
	type transform = NniTypes.edge_state
	let compose = NniGops.compose
	let decomp x y c =
		let lxy = NniGops.binload_node_xor c in
		let lx, ly = NniGops.pair_split lxy in
		((false, lx), x), ((false, ly), y)
	let solver = NniGops.node_solve_xor
end;;

module AND = GroBdd.IBOP(AND_M);;
module XOR = GroBdd.IBOP(XOR_M);;


module TACX_M : TaggedSubdag.MODELE with
		type node = Bitv.t
	and	type edge = NniTypes.edge_state
	and type leaf = unit
	and type tag  = TacxTypes.tag
=
struct
	
	type node = Bitv.t
	type edge = NniTypes.edge_state
	type leaf = unit
	type tag  = TacxTypes.tag

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push = NniGops.tacx_push
	let pull = NniGops.tacx_pull
	let compose = NniGops.compose
	
	let pull_node = NniGops.tacx_pull_node
	
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
	include TaggedSubdag.MODULE(TACX_M)
	let dumpfile man edges target =
		let strman = Udag.STree.newman() in
		let stredges = dump man strman edges in
		Udag.STree.dumpfile strman stredges target
	
	let loadfile target =
		let strman, stredges = Udag.STree.loadfile target in
		let man = newman () in
		let edges = load man strman stredges in
		man, edges

	let stree_dump man edges =
		let strman = Udag.STree.newman() in
		let stredges = dump man strman edges in
		Udag.STree.dump strman stredges

	let stree_load stree =
		let strman, stredges = Udag.STree.load stree in
		let man = newman () in
		let edges = load man strman stredges in
		man, edges
end

let ( *! ) man x y = TACX.push man TacxTypes.Cons x y
let ( &! ) man x y = TACX.push man TacxTypes.And x y
and ( ^! ) man x y = TACX.push man TacxTypes.Xor x y

module EVAL =
struct
	module EVAL_VISITOR =
	struct
		type xnode = GroBdd.edge
		type xedge = GroBdd.edge
		type cons = xedge -> xedge -> xedge
		type extra = cons * cons * cons (* (a, c, x) *)

		let do_leaf _ () = default_leaf
		let do_node (a, c, x) = Extra.(NniGops.binload_tacx >> NniGops.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let merge = TacxTypes.(match tag with And -> a | Cons -> c | Xor -> x) in
			Utils.MNode (fun nodeX nodeY -> merge (NniGops.compose edgeX nodeX) (NniGops.compose edgeY nodeY))))
		let do_edge _ = NniGops.compose
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

module PURE_OF_CP =
struct
	module EVAL_VISITOR =
	struct
		type xnode = GroBdd.edge
		type xedge = GroBdd.edge
		type cons = xedge -> xedge -> xedge
		type extra = GroBdd.manager

		let do_leaf _ () = default_leaf
		let mu_nni_of_cp = function
			| CpTypes.S -> NniTypes.S false
			| CpTypes.P -> NniTypes.P (false, None)
		let nni_of_cp (b, u) = (b, List.map mu_nni_of_cp u)
		let do_node extra = Extra.(CpGops.binload_node >> CpGops.node_split >> (fun (edgeX, edgeY) ->
			let edgeX = nni_of_cp edgeX
			and edgeY = nni_of_cp edgeY in
			let result = Utils.MNode NniGops.(fun nodeX nodeY ->
				GroBdd.push extra (compose edgeX nodeX) (compose edgeY nodeY)) in
			(result : (xnode, xnode -> xnode -> xnode) Utils.merge)))

		let do_edge _ e = NniGops.compose (nni_of_cp e)
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
(*		Tree.Node [Tree.Leaf "grobdd:"; GroBdd.dump_stats man.grobdd]; *)
(*		Tree.Node [Tree.Leaf "man_cp:"; Cp.TACX.dump_stats man.man_cp]; *)
(*		Tree.Node [Tree.Leaf "mannni:"; TACX.dump_stats man.mannni]; *)
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
		let mu_nni_of_cp = function
			| CpTypes.S -> NniTypes.S false
			| CpTypes.P -> NniTypes.P (false, None)
		let nni_of_cp (b, u) = (b, List.map mu_nni_of_cp u)
		let do_node extra = Extra.(CpGops.binload_tacx >> CpGops.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let edgeX = nni_of_cp edgeX
			and edgeY = nni_of_cp edgeY in
			let result = Utils.MNode NniGops.(fun nodeX nodeY ->
				TACX.push extra tag (compose edgeX nodeX) (compose edgeY nodeY)) in
			(result : (xnode, xnode -> xnode -> xnode) Utils.merge)))

		let do_edge _ e = NniGops.compose (nni_of_cp e)
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
(*		Tree.Node [Tree.Leaf "grobdd:"; GroBdd.dump_stats man.grobdd]; *)
(*		Tree.Node [Tree.Leaf "man_cp:"; Cp.TACX.dump_stats man.man_cp]; *)
(*		Tree.Node [Tree.Leaf "mannni:"; TACX.dump_stats man.mannni]; *)
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

		let cswap b (x, y) = if b then (y, x) else (x, y)
		let add (x, y) (x', y') = (BigInt.(+) x x', BigInt.(+) y y')
		let mushift (n, p, a) x =
			assert(n>=0);
			assert(p>=0);
			assert(a>=0);
			if a = 0
			then (BigInt.shift_left x p)
			else (BigInt.shift_left BigInt.unit (n-1))
		let shift n (x, y) = (mushift n x, mushift n y)
		let p = function NniTypes.P(false, _) -> true | _ -> false
		let a = function NniTypes.P(true, _) -> true | _ -> false

		let do_leaf () () = (BigInt.zero, BigInt.unit)
		let do_node ()    = Extra.(NniGops.binload_node >> NniGops.node_split >> (fun ((bX, lX), (bY, lY)) ->
			let n = List.length lX in
			assert(n = List.length lY);
			let pX = MyList.count p lX
			and aX = MyList.count a lX
			and pY = MyList.count p lY
			and aY = MyList.count a lY in
			Utils.MNode (fun x y -> add (shift (n, pX, aX) (cswap bX x)) (shift (n, pY, aY) (cswap bY y)))))
		let do_edge () (b, l) (x, y) =
			let x = if b then y else x in
			let n = MyList.count p l in
			BigInt.shift_left x n

	end

	module CntSat = GroBdd.NODE_VISITOR(CntSat_VISITOR)

	let newman man =
		CntSat.newman man ()
	
	let dump_stats = CntSat.dump_stats
	
end

(*
module GroBdd_AllSat =
struct
	module AllSat_VISITOR =
	struct
		type xnode = (bool option list list) * (bool option list list)
		type xedge = (bool option list list)
		type extra = unit

		let cswap b (x, y) = if b then (y, x) else (x, y)
		let p = function NniTypes.P -> true | _ -> false
		let compose =
			let rec aux carry = function
			  | ([], []) -> List.rev carry
			  | (NniTypes.S::x', y::y') -> aux (y::carry) (x', y')
			  | (NniTypes.P::x', y') -> aux (None::carry) (x', y')
			  | _ -> assert false
			in function
			  | None	-> fun deco elem -> aux [] (deco, elem)
			  | Some b	-> fun deco elem -> aux [Some b] (deco, elem)

		let shift deco (x, y) = (List.map (compose (Some false) deco) x, List.map (compose (Some true) deco) y)
		let add (x, y) (x', y') = (x@x', y@y')

		let do_leaf (():extra) (():GroBdd.M.leaf) = (([], [[]]):xnode)
		let do_node ()    = Extra.(NniGops.binload_node >> NniGops.node_split >> (fun ((bX, lX), (bY, lY)) ->
			Utils.MNode (fun x y -> add (shift lX (cswap bX x)) (shift lY (cswap bY y)))))
		let do_edge (():extra) ((b, l):GroBdd.M.edge) ((x, y):xnode) =
			let x = if b then y else x in
			List.map (compose None l) x
	end

	module AllSat = GroBdd.NODE_VISITOR(AllSat_VISITOR)

	let newman man =
		AllSat.newman man ()
	
	let dump_stats = AllSat.dump_stats
	
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
