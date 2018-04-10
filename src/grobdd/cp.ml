(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

let strdump_node = STD.bitv_hexa
let strload_node = Extra.(STL.bitv_hexa >> CpGops.binload_node >> CpGops.node_split)

let strdump_tacx = STD.bitv_hexa
let strload_tacx = Extra.(STL.bitv_hexa >> CpGops.binload_tacx >> CpGops.tacx_split)

let strdump_edge = Extra.(CpGops.bindump_edge >> STD.bitv_hexa)
let strload_edge = Extra.(STL.bitv_hexa >> CpGops.binload_edge)

let dot_of_edge_aux color (b, l) =
	"[label = \""^(String.concat "" ((if b then "-" else "+")::(List.map CpGops.strdump_uniq_elem l)))^"\"; color=\""^color^"\"];"

let dot_of_edge = dot_of_edge_aux "black"

let dot_of_node node =
	let e0, e1 = CpGops.binload_node node |> CpGops.node_split in
	"", (dot_of_edge_aux "red" e0), (dot_of_edge_aux "blue" e1)

let default_leaf = ((false, []), Utils.Leaf ())

let strdump_leaf = (fun () -> Tree.Node [])
let strload_leaf = (function Tree.Node [] -> default_leaf | _ -> assert false)

module GroBdd_M : Subdag.MODELE with
		type node = Bitv.t
	and	type edge = CpTypes.edge_state
	and type leaf = unit
=
struct
	
	type node = Bitv.t
	type edge = CpTypes.edge_state
	type leaf = unit

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push : ('t -> 'i) -> 't e -> 't e -> ('t e, edge * 't n) Utils.merge = CpGops.node_push_cons
	let pull	= CpGops.node_pull
	let compose = CpGops.compose
	
	let pull_node	= CpGops.node_pull_node
	
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

let make_const b n = GroBdd.push_leaf (b, MyList.ntimes CpTypes.P n) ();;

let make_ident man b n = GroBdd.push man (make_const b n) (make_const (not b) n);;

let arity ((_, l), _) = List.length l

let push_pass ((b, l), i) = ((b, CpTypes.P::l), i)

let no ((b, l), i) = ((not b, l), i)
let cno b' ((b, l), i) = ((b' <> b, l), i)

let is_root = function
	| ((b, l), Utils.Leaf ()) -> Some b
	| _ -> None 

let get_root b ((_, l), _) = ((b, MyList.ntimes CpTypes.P (List.length l)), Utils.Leaf ())

let (=??) (ex, ix) (ey, iy) = match ix, iy with
	| Utils.Leaf (), Utils.Leaf () -> ex = ey
	| Utils.Node nx, Utils.Node ny -> (nx == ny) && (ex = ey)
	| _ -> false

module AND_M : GroBdd.MODELE_IBOP =
struct
	type t = Bitv.t
	let compare = Pervasives.compare
	type transform = CpTypes.edge_state
	let compose = CpGops.compose
	let decomp x y c =
		let ((bx, by), lxy) = CpGops.binload_node_and c in
		let lx, ly = CpGops.split_pair lxy in
		((bx, lx), x), ((by, ly), y)
	let solver = CpGops.node_solve_and
end;;

module XOR_M : GroBdd.MODELE_IBOP =
struct
	type t = Bitv.t
	let compare = Pervasives.compare
	type transform = CpTypes.edge_state
	let compose = CpGops.compose
	let decomp x y c =
		let lxy = CpGops.binload_node_xor c in
		let lx, ly = CpGops.split_pair lxy in
		((false, lx), x), ((false, ly), y)
	let solver = CpGops.node_solve_xor
end;;

module AND = GroBdd.IBOP(AND_M);;
module XOR = GroBdd.IBOP(XOR_M);;


module TACX_M : TaggedSubdag.MODELE with
		type node = Bitv.t
	and	type edge = CpTypes.edge_state
	and type leaf = unit
	and type tag  = TacxTypes.tag
=
struct
	
	type node = Bitv.t
	type edge = CpTypes.edge_state
	type leaf = unit
	type tag  = TacxTypes.tag

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push = CpGops.tacx_push
	let pull = CpGops.tacx_pull
	let compose = CpGops.compose
	
	let pull_node = CpGops.tacx_pull_node
	
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
	module VISITOR =
	struct
		type xnode = GroBdd.edge
		type xedge = GroBdd.edge
		type cons = xedge -> xedge -> xedge
		type extra = cons * cons * cons (* (a, c, x) *)

		let do_leaf _ () = default_leaf
		let do_node (a, c, x) = Extra.(CpGops.binload_tacx >> CpGops.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let merge = TacxTypes.(match tag with And -> a | Cons -> c | Xor -> x) in
			Utils.MNode (fun nodeX nodeY -> merge (CpGops.compose edgeX nodeX) (CpGops.compose edgeY nodeY))))
		let do_edge _ = CpGops.compose
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
(*		Tree.Node [Tree.Leaf "grobdd:"; GroBdd.dump_stats man.grobdd]; *)
		Tree.Node [Tree.Leaf "andman:"; AND.dump_stats man.andman];
		Tree.Node [Tree.Leaf "xorman:"; XOR.dump_stats man.xorman];
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
		let shift n (x, y) = (BigInt.shift_left x n, BigInt.shift_left y n)
		let p = function CpTypes.P -> true | _ -> false

		let do_leaf () () = (BigInt.zero, BigInt.unit)
		let do_node ()    = Extra.(CpGops.binload_node >> CpGops.node_split >> (fun ((bX, lX), (bY, lY)) ->
			let nX = MyList.count p lX
			and nY = MyList.count p lY in
			Utils.MNode (fun x y -> add (shift nX (cswap bX x)) (shift nY (cswap bY y)))))
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

module AllSat =
struct
	module AllSat_VISITOR =
	struct
		type xnode = (bool option list list) * (bool option list list)
		type xedge = (bool option list list)
		type extra = unit

		let cswap b (x, y) = if b then (y, x) else (x, y)
		let p = function CpTypes.P -> true | _ -> false
		let compose =
			let rec aux carry = function
			  | ([], []) -> List.rev carry
			  | (CpTypes.S::x', y::y') -> aux (y::carry) (x', y')
			  | (CpTypes.P::x', y') -> aux (None::carry) (x', y')
			  | _ -> assert false
			in function
			  | None	-> fun deco elem -> aux [] (deco, elem)
			  | Some b	-> fun deco elem -> aux [Some b] (deco, elem)

		let shift deco (x, y) = (List.map (compose (Some false) deco) x, List.map (compose (Some true) deco) y)
		let add (x, y) (x', y') = (x@x', y@y')

		let do_leaf (():extra) (():GroBdd.M.leaf) = (([], [[]]):xnode)
		let do_node ()    = Extra.(CpGops.binload_node >> CpGops.node_split >> (fun ((bX, lX), (bY, lY)) ->
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

module PartEval =
struct
	module Eval_VISITOR : GroBdd.MODELE_EVAL with
			type pars = bool option list option
		and type back = GroBdd.M.edge
	=
	struct
		type pars = bool option list option
		type back = GroBdd.M.edge

		let pars gid pars ((be, le), ie) = match pars with
		| None -> Utils.MStop ((be, le), ie)
		| Some pars ->
			let lb, lpe = List.split(List.map2(fun p e -> match p, e with
				| None, CpTypes.P -> Some CpTypes.P, None
				| None, CpTypes.S -> Some CpTypes.S, Some(None, CpTypes.S)
				| Some _, CpTypes.P -> None, None
				| Some b, CpTypes.S -> Some CpTypes.S, Some(Some b, CpTypes.S)) pars le) in
			let lb = MyList.list_of_oplist lb
			and pars, le = List.split(MyList.list_of_oplist lpe) in
			if List.for_all (function None -> true | Some _ -> false) pars
			then Utils.MStop (CpGops.compose (false, lb) ((be, le), ie))
			else match pars with
				| [] -> assert false
				| h::t -> match h with
					| None       -> Utils.MPull ((false, lb), Some t, Some t)
					| Some false -> Utils.Go0 ((false, lb), Some t)
					| Some true	 -> Utils.Go1 ((false, lb), Some t)

		let back = CpGops.compose
	end

	include GroBdd.EVAL(Eval_VISITOR)

end

module PURE_TO_BRYANT =
struct
	module CONS_VISITOR =
	struct
		type xedge = Bryant.GroBdd.edge
		type xresi = int
		type extra = Bryant.GroBdd.manager

		let do_edge _ ((b, l), i) = match i with
			| Utils.Leaf () ->
			(
				assert(List.for_all (function CpTypes.P -> true | _ -> false) l);
				Utils.MEdge ((b, (List.length l, 0)), Utils.Leaf())
			)
			| Utils.Node n ->
			(
				let rec aux carry = function
					| [] -> assert false
					| head::tail as liste -> match head with
						| CpTypes.P -> aux (1+carry) tail
						| _			-> carry, liste
				in 
				let shift, l = aux 0 l in
				Utils.MNode (shift, ((b, l), Utils.Node n))
			)

		let push = Bryant.GroBdd.push

		let compose _ (shift:xresi) ((b, (x, y)), i) = ((b, (shift+x, y)), i)
	end

	include GroBdd.CONS_VISITOR(CONS_VISITOR)

end

module PURE_TO_ZDD =
struct
	module CONS_VISITOR =
	struct
		type xedge = Zdd.GroBdd.edge
		type xresi = unit
		type extra = Zdd.GroBdd.manager

		let do_edge _ ((b, l), i) = match i with
			| Utils.Leaf () ->
			(
				assert(List.for_all (function CpTypes.P -> true | _ -> false) l);
				Utils.MEdge ((List.length l, 0), Utils.Leaf b)
			)
			| Utils.Node n ->
			(
				Utils.MNode ((), ((b, l), Utils.Node n))
			)

		let push = Zdd.GroBdd.push

		let compose _ () edge = edge
	end

	include GroBdd.CONS_VISITOR(CONS_VISITOR)

end
