(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

let strdump_node = STD.bitv_hexa
let strload_node = Extra.(STL.bitv_hexa >> CpxDumpLoad.binload_node >> CpxUtils.block_split)

let strdump_tacx = STD.bitv_hexa
let strload_tacx = Extra.(STL.bitv_hexa >> CpxDumpLoad.binload_tacx >> CpxUtils.tacx_split)

let strdump_edge = Extra.(CpxDumpLoad.bindump_edge >> STD.bitv_hexa)
let strload_edge = Extra.(STL.bitv_hexa >> CpxDumpLoad.binload_edge)

let dot_of_edge_aux color block =
	"[label = \""^(CpxDumpLoad.block_to_pretty block)^"\"; color=\""^color^"\"];"

let dot_of_edge = dot_of_edge_aux "black"

let dot_of_node node =
	let e0, e1 = node |> CpxDumpLoad.binload_node |> CpxUtils.block_split in
	"", (dot_of_edge_aux "red" e0), (dot_of_edge_aux "blue" e1)

let default_leaf = CpxTypes.({neg = false; shift = false; sub = []}, Utils.Leaf ())

let strdump_leaf = (fun () -> Tree.Node [])
let strload_leaf = (function Tree.Node [] -> default_leaf | _ -> assert false)

module GroBdd_M : Subdag.MODELE with
		type node = Bitv.t
	and	type edge = CpxTypes.edge_state
	and type leaf = unit
=
struct
	
	type node = Bitv.t
	type edge = CpxTypes.edge_state
	type leaf = unit

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push : ('t -> 'i) -> 't e -> 't e -> ('t e, edge * 't n) Utils.merge = CpxGops.node_push_cons
	let pull : ('t -> 'i) -> 't e -> ('t e * 't e, 't n -> ('t e * 't e)) Utils.merge = fun gid edge -> match CpxGops.node_pull gid edge with
		| Utils.MEdge e -> Utils.MEdge e
		| Utils.MNode f -> Utils.MNode (fun (node, i1, i2) -> f (CpxDumpLoad.binload_node node, i1, i2))
	let compose = CpxUtils.compose
	
	let pull_node _ (n, i0, i1) = CpxUtils.node_split (CpxDumpLoad.binload_node n, i0, i1)
	
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

let make_const b n = GroBdd.push_leaf CpxTypes.{neg = b; shift = false; sub = MyList.ntimes P n } ();;

let make_ident man b n = GroBdd.push man (make_const b n) (make_const (not b) n);;

let arity block = CpxTypes.(List.length block.sub)

let push_pass = CpxUtils.push_P

let no = CpxUtils.neg

let is_root = CpxUtils.node_is_const

let get_root = CpxUtils.get_root

let (=??) (ex, ix) (ey, iy) = match ix, iy with
	| Utils.Leaf (), Utils.Leaf () -> ex = ey
	| Utils.Node nx, Utils.Node ny -> (nx == ny) && (ex = ey)
	| _ -> false

module AND_M : GroBdd.MODELE_IBOP =
struct
	type t = Bitv.t
	let compare = Pervasives.compare
	type transform = CpxTypes.edge_state
	let compose = CpxUtils.compose
	let decomp x y c = (CpxDumpLoad.binload_node c, x, y) |> CpxUtils.node_split
	let solver = CpxGops.node_push_and
end;;

module XOR_M : GroBdd.MODELE_IBOP =
struct
	type t = Bitv.t
	let compare = Pervasives.compare
	type transform = CpxTypes.edge_state
	let compose = CpxUtils.compose
	let decomp x y c = (CpxDumpLoad.binload_node c, x, y) |> CpxUtils.node_split
	let solver = CpxGops.node_push_xor
end;;

module AND = GroBdd.IBOP(AND_M);;
module XOR = GroBdd.IBOP(XOR_M);;


module TACX_M : TaggedSubdag.MODELE with
		type node = Bitv.t
	and	type edge = CpxTypes.edge_state
	and type leaf = unit
	and type tag  = TacxTypes.tag
=
struct
	
	type node = Bitv.t
	type edge = CpxTypes.edge_state
	type leaf = unit
	type tag  = TacxTypes.tag

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push = CpxGops.tacx_push
	let pull = CpxGops.tacx_pull
	let compose = CpxUtils.compose
	
	let pull_node = CpxGops.tacx_pull_node
	
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
		let do_node (a, c, x) = Extra.(CpxDumpLoad.binload_tacx >> CpxUtils.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let merge = NniTypes.(match tag with TacxTypes.And -> a | TacxTypes.Cons -> c | TacxTypes.Xor -> x) in
			Utils.MNode (fun nodeX nodeY -> merge (CpxUtils.compose edgeX nodeX) (CpxUtils.compose edgeY nodeY))))
		let do_edge _ = CpxUtils.compose
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
		let do_node (a, c, x) = Extra.(CpxDumpLoad.binload_tacx >> CpxUtils.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let merge = TacxTypes.(match tag with And -> a | Cons -> c | Xor -> x) in
			Utils.MNode (fun nodeX nodeY -> merge (CpxUtils.compose edgeX nodeX) (CpxUtils.compose edgeY nodeY))))
		let do_edge _ = CpxUtils.compose
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
			| CpTypes.S -> CpxTypes.S
			| CpTypes.P -> CpxTypes.P
		let cpx_of_cp (b, u) = CpxTypes.{neg = b; shift = false; sub = List.map mu_cpx_of_cp u}
		let do_node extra = Extra.(CpGops.binload_node >> CpGops.node_split >> (fun (edgeX, edgeY) ->
			let edgeX = cpx_of_cp edgeX
			and edgeY = cpx_of_cp edgeY in
			let result = Utils.MNode CpxGops.(fun nodeX nodeY ->
				GroBdd.push extra (CpxUtils.compose edgeX nodeX) (CpxUtils.compose edgeY nodeY)) in
			(result : (xnode, xnode -> xnode -> xnode) Utils.merge)))

		let do_edge _ e = CpxUtils.compose (cpx_of_cp e)
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
			| CpTypes.S -> CpxTypes.S
			| CpTypes.P -> CpxTypes.P
		let cpx_of_cp (b, u) = CpxTypes.{neg = b; shift = false; sub = List.map mu_cpx_of_cp u}
		let do_node extra = Extra.(CpGops.binload_tacx >> CpGops.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let edgeX = cpx_of_cp edgeX
			and edgeY = cpx_of_cp edgeY in
			let result = Utils.MNode CpxGops.(fun nodeX nodeY ->
				TACX.push extra tag (CpxUtils.compose edgeX nodeX) (CpxUtils.compose edgeY nodeY)) in
			(result : (xnode, xnode -> xnode -> xnode) Utils.merge)))

		let do_edge _ e = CpxUtils.compose (cpx_of_cp e)
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

		let cswap b (x, y) = if b then (y, x) else (x, y)
		let add (x, y) (x', y') = (BigInt.(+) x x', BigInt.(+) y y')
		let shift n (x, y) = (BigInt.shift_left x n, BigInt.shift_left y n)
		let p block = CpxTypes.(MyList.count (function P -> true | _ -> false) block.sub)

		let do_leaf () () = (BigInt.zero, BigInt.unit)
		let x block =
			match CpxUtils.classify block |> snd with
			| None -> (BigInt.zero, BigInt.zero)
			| Some maxX ->
				let liste = MyList.init (maxX+1) CpxTypes.(fun i -> List.fold_left (fun (lvl, under) -> function
					| S -> (lvl, under+1)
					| P -> (lvl, under)
					| X(_, j) -> if i = j then (lvl+1, under) else if i < j then (lvl, under+1) else (lvl, under)) (0, 0) block.sub) in
				let liste = List.map (fun (lvl, under) -> BigInt.(shift_left (pow2 lvl - unit) under)) liste in
				let rec aux0 (if0, if1) = function
					| [] -> (if0, if1)
					| [x0] -> BigInt.(if0 + x0, if1)
					| x0::x1::tail -> aux0 BigInt.(if0 + x0, if1 + x1) tail
				in
				CpxTypes.(Tools.cswap (not block.shift) (aux0 BigInt.(zero, zero) liste))
		let do_node ()    = Extra.(CpxDumpLoad.binload_node >> CpxUtils.block_split >> CpxTypes.(fun (blockX, blockY) ->
			let nX = p blockX in
			let x01 = x blockX in
			let nY = p blockY in
			let y01 = x blockY in
			Utils.MNode (fun x y -> add (shift nX (cswap blockX.neg (add x01 x))) (shift nY (cswap blockY.neg (add y01 y))))))
		let do_edge () block x01 =
			let x0, x1 = add x01 (x block) in
			BigInt.shift_left (if block.CpxTypes.neg then x1 else x0) (p block)

	end

	module CntSat = GroBdd.NODE_VISITOR(CntSat_VISITOR)

	let newman man =
		CntSat.newman man ()
	
	let dump_stats = CntSat.dump_stats
	
end

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
			let _, maxX = CpxUtils.classify block in
			match maxX with
			| None -> ([], [])
			| Some maxX ->
				let llist = MyList.init (maxX+1) CpxTypes.(fun i ->
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
				Tools.cswap block.CpxTypes.shift if01

		let compose =
			let rec aux carry = CpxTypes.(function
			  | ([], [])			 -> List.rev carry
			  | (S      ::x', y::y') -> aux (y            ::carry) (x', y')
			  | (P      ::x', y'   ) -> aux (None		  ::carry) (x', y')
			  | (X(b, _)::x', y'   ) -> aux ((Some(not b))::carry) (x', y')
			  | _ -> assert false)
			in fun block elem -> aux [] (block.CpxTypes.sub, elem)

		let map_compose block (liste0, liste1) = (List.map (compose block) liste0, List.map (compose block) liste1)

		let map_push b (liste0, liste1) = (List.map (fun x -> (Some b)::x) liste0, List.map (fun x -> (Some b)::x) liste1)

		let add (x, y) (x', y') = (x@x', y@y')

		let shift block l01 = CpxTypes.(cswap block.neg (add (x block) (map_compose block l01)))

		let do_leaf (():extra) (():GroBdd.M.leaf) = (([], [[]]):xnode)
		let do_node ()    = Extra.(CpxDumpLoad.binload_node >> CpxUtils.block_split >> CpxTypes.(fun (blockX, blockY) ->
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
			let (ex, ix), pars = CpxGops.assign pars (ex, ix) in
			match pars with
			| None -> Utils.MStop (ex, ix)
			| Some [] -> assert false
			| Some (head::tail) -> match head with
				| None			-> Utils.MPull	(ex, (Some tail), (Some tail))
				| Some false	-> Utils.Go0	(ex, (Some tail))
				| Some true		-> Utils.Go1	(ex, (Some tail))

		let back = CpxUtils.compose
	end

	include GroBdd.EVAL(Eval_VISITOR)

end

module AND_ME : GroBdd.MODELE_IBOP_EVAL =
struct
	type compact = Bitv.t
	type residual = CpxTypes.edge_state
	type eval = bool option list

	let solver gid x y = CpxGops.node_push_ande gid (x, y)
	
	let decomp x y c = (CpxDumpLoad.binload_node c, x, y) |> CpxUtils.node_split

	let solver' gid c x' y' =
		let x, y = CpxDumpLoad.binload_node c |> CpxUtils.block_split in
		let x = match x' with
			| Utils.MNode gtree -> CpxUtils.node_reduce (x, gtree)
			| Utils.MEdge edge  ->
			(
				(*print_string "x' = MEdge edge -> edge: "; print_string(CpxDumpLoad.edge_dummydump edge); print_newline();*)
				CpxUtils.compose x edge
			)
		and y = match y' with
			| Utils.MNode gtree -> CpxUtils.node_reduce (y, gtree)
			| Utils.MEdge edge  ->
			(
				(*print_string "y' = MEdge edge -> edge: "; print_string(CpxDumpLoad.edge_dummydump edge); print_newline();*)
				CpxUtils.compose y edge
			)
		in
		(*print_string "[0] solver'"; print_newline();
		print_string "\tex: "; print_string(CpxDumpLoad.edge_dummydump x); print_newline();
		print_string "\tey: "; print_string(CpxDumpLoad.edge_dummydump y); print_newline();*)
		solver gid x y
	
	let eval set (ex, ix) =
		let (ex, ix), set = CpxGops.assign (Some set) (ex, ix) in
		(ex, (set, ix))
	
	let read = function
		| [] -> assert false
		| head::tail -> match head with
			| None		 -> Utils.MPull (tail, tail)
			| Some false -> Utils.Go0 tail
			| Some true  -> Utils.Go1 tail

	let compose = CpxUtils.compose
end;;

module XOR_ME : GroBdd.MODELE_IBOP_EVAL =
struct
	type compact = Bitv.t
	type residual = CpxTypes.edge_state
	type eval = bool option list

	let solver gid x y = match CpxGops.node_push_xor gid (x, y) with
		| Utils.MEdge (edge, gtree) -> Utils.M3Edge (edge, (None, gtree))
		| Utils.MNode (r, (c, x, y)) -> Utils.M3Node (r, (c, (None, x), (None, y)))
(*		CpxTypes.(
			let return () = Utils.M3Node (r, (c, (None, x), (None, y))) in
			if List.length c.subXY >= 1
			then match c.subXY with
			| [] -> assert false
			| (X(b, 0), X(b', 0))::subXY when b <> b' ->
			(
				let ex, ey = CpxUtils.block_split {negX = c.negX <> c.negY <> c.shiftY; negY = c.negY <> c.negX <> c.shiftX; shiftX = x.shiftX; shiftY = c.shiftY; subXY } in
				let x = (CpxUtils.reduce ex, (None, x))
				and y = (CpxUtils.reduce ey, (None, y)) in
				Utils.M3Cons (r, Tools.cswap b (y, x))
			)
			| _ -> result()
			else (result())
		)
*)

	let decomp x y c = (CpxDumpLoad.binload_node c, x, y) |> CpxUtils.node_split

	let solver' gid c x' y' =
		let x, y = CpxDumpLoad.binload_node c |> CpxUtils.block_split in
		let x = match x' with
			| Utils.MNode gtree -> CpxUtils.node_reduce (x, gtree)
			| Utils.MEdge edge  -> CpxUtils.compose x edge
		and y = match y' with
			| Utils.MNode gtree -> CpxUtils.node_reduce (y, gtree)
			| Utils.MEdge edge  -> CpxUtils.compose y edge
		in solver gid x y
	
	let eval set (ex, ix) =
		let (ex, ix), set = CpxGops.assign (Some set) (ex, ix) in
		(ex, (set, ix))
	
	let read = function
		| [] -> assert false
		| head::tail -> match head with
			| None		 -> Utils.MPull (tail, tail)
			| Some false -> Utils.Go0 tail
			| Some true  -> Utils.Go1 tail

	let compose = CpxUtils.compose
end;;
module ANDE = GroBdd.IBOP_EVAL(AND_ME)
module XORE = GroBdd.IBOP_EVAL(XOR_ME)

module EVALE =
struct
	module EVAL_VISITOR =
	struct
		type xnode = GroBdd.edge
		type xedge = GroBdd.edge
		type cons = xedge -> xedge -> xedge
		type extra = cons * cons * cons (* (a, c, x) *)

		let do_leaf _ () = default_leaf
		let do_node (a, c, x) = Extra.(CpxDumpLoad.binload_tacx >> CpxUtils.tacx_split >> (fun (tag, edgeX, edgeY) ->
			let merge = NniTypes.(match tag with TacxTypes.And -> a | TacxTypes.Cons -> c | TacxTypes.Xor -> x) in
			Utils.MNode (fun nodeX nodeY -> merge (CpxUtils.compose edgeX nodeX) (CpxUtils.compose edgeY nodeY))))
		let do_edge _ = CpxUtils.compose
	end

	module EVAL = TACX.NODE_VISITOR(EVAL_VISITOR)

	type manager = {
		grobdd : GroBdd.manager;
		andman : ANDE.manager;
		xorman : XORE.manager;
		theman : EVAL.manager;
		calc   : TACX.edge -> GroBdd.edge
	}

	let newman tacx man =
		let c = GroBdd.push man in
		let aman, a = ANDE.newman man
		and xman, x = XORE.newman man in
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
		Tree.Node [Tree.Leaf "andman:"; ANDE.dump_stats man.andman];
		Tree.Node [Tree.Leaf "xorman:"; XORE.dump_stats man.xorman];
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
			let (ex, ix), pars = CpxGops.assign (Some pars) (ex, ix) in
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


module TACX_PROPA_M : TACX.MODELE_IUOP2 with
	type eval = bool option list
=
struct
	type eval = bool option list

	let eval pars (ex, ix) =
		assert(List.length pars = CpxUtils.block_size ex);
		let (ex, ix), pars = CpxGops.assign (Some pars) (ex, ix) in
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
			| head::mess' -> match head with
				| None -> Utils.MPull (TacxTypes.Cons, mess', mess')
				| Some false -> Utils.Go0 mess'
				| Some true  -> Utils.Go1 mess'

	type eog = TACX.M.edge * (eval option * TACX.G.tree)

	let solver = CpxGops.tacx_propa

end

module TACX_PROPA = TACX.IUOP2(TACX_PROPA_M)

module PURE_TO_BRYANT =
struct
	module CONS_VISITOR =
	struct
		type xedge = Bryant.GroBdd.edge
		type xresi = bool * int
		type extra = Bryant.GroBdd.manager

		let do_edge _ (block, i) = match CpxUtils.block_is_const block with
			| Some b ->
			(
				Utils.MEdge ((b, (CpxUtils.block_size block, 0)), Utils.Leaf())
			)
			| None ->
			CpxTypes.(
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
