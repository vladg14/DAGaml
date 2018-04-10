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
		type edge = NniTypes.edge_state
		type node = unit

		let strdump_leaf = StrDump.unit
		let strdump_edge = NniBDumpLoad.strdump_edge
		let strdump_node = StrDump.unit

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = NniBDumpLoad.(bindump_edge, binload_edge)
		let o3s_node = BinO3.unit

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node NniBDumpLoad.(bindump_node, binload_node) o3s_leaf o3s_ident

		let __check_reverse__ = false

	end

	module M1 =
	struct
		module M = M0

		let arity     = NniBGops.arity
		let compose   = NniBGops.compose
		let push_node = NniBGops.cons_cons
		let pull_node = NniBGops.node_pull
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
			"[label = \""^(NniBDumpLoad.strdump_edge edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node _ = ""
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile = TO_DOT.dotfile

	module AND_M =
	struct
		module M = G1
		
		let solver = NniBGops.cons_and
	end

	module AND = BinUbdagTC.BINOP(AND_M)

	module XOR_M =
	struct
		module M = G1
		
		let solver = NniBGops.cons_xor
	end

	module XOR = BinUbdagTC.BINOP(XOR_M)

	type manager = {
		cons : G1.manager;
		and_ : AND.manager;
		xor_ : XOR.manager;
		solve_cons :                  G0.node' -> G0.edge';
		solve_and  :                  G0.node' -> G0.edge';
		solve_xor  :                  G0.node' -> G0.edge';
		solve_tacx : TacxTypes.tag -> G0.node' -> G0.edge';
	}

	let get_cons man = man.cons

	let dump_stats man = Tree.Node [
		Tree.Node [Tree.Leaf "cons"; G1.dump_stats man.cons];
		Tree.Node [Tree.Leaf "and "; AND.dump_stats man.and_];
		Tree.Node [Tree.Leaf "xor "; XOR.dump_stats man.xor_];
	]

	let push man = man.solve_cons

	let makeman hsize =
		let cons = G1.makeman hsize in
		let and_ = AND.makeman cons hsize
		and xor_ = XOR.makeman cons hsize in
		let solve_cons = G1.push cons
		and solve_and  = AND.map and_
		and solve_xor  = XOR.map xor_ in
		let solve_tacx = TacxTypes.(function
			| And  -> solve_and
			| Cons -> solve_cons
			| Xor  -> solve_xor
		) in
		{cons; and_; xor_; solve_cons; solve_and; solve_xor; solve_tacx}

    let default_newman_hsize = 10000

	let newman () = makeman default_newman_hsize
(*
	module PEVAL_MODELE =
	struct
		module M = G1

		type peval = bool option list

		let dump_peval = BinDump.bool_option_list
		let load_peval = BinLoad.bool_option_list

		type next = peval option * M.G.ident

		type next' = next M.M.M.next'
		type edge' = next M.M.M.edge'
		type node' = next M.M.M.node'

		let eval_edge : peval -> edge' -> edge' = NniBGops.assign 
		let eval_node : peval -> node' -> (edge', node') Utils.merge  = function
			| [] -> assert false
			| head::peval -> match head with
				| None       -> fun ((), edge0, edge1) ->
					Utils.MNode((), eval_edge peval edge0, eval_edge peval edge1)
				| Some false -> fun((), edge0, _) ->
					Utils.MEdge(eval_edge peval edge0)
				| Some true  -> fun((), _, edge1) ->
					Utils.MEdge(eval_edge peval edge1)
	end

	module PEVAL = BinUbdagTC.PEVAL(PEVAL_MODELE)
*)
	module CntSat_MODELE =
	struct
		module M = G0

		type xnode  = BigInt.big_int * BigInt.big_int
		type xedge  = BigInt.big_int
		type extra  = unit

		type next' = (unit -> xnode) M.M.next'
		type edge' = (unit -> xnode) M.M.edge'
		type node' = (unit -> xnode) M.M.node'

		let cswap b (x, y) = if b then (y, x) else (x, y)
		let add (x, y) (x', y') = (BigInt.(+) x x', BigInt.(+) y y')
		let shift n (x, y) = (BigInt.shift_left x n, BigInt.shift_left y n)
		let p = function NniTypes.P(false, _) -> true | _ -> false
		let a = function NniTypes.P(true , _) -> true | _ -> false

		let rec_next = function
			| Utils.Leaf () -> (BigInt.zero, BigInt.unit)
			| Utils.Node node -> node()

		let rec_edge ((b, l), next) =
			if List.exists a l
			then
			(
				let n = BigInt.shift_left BigInt.unit ((List.length l) - 1) in
				(n, n)
			)
			else (cswap b (shift (MyList.count p l) (rec_next next)))

		let map_node () ((), edge0, edge1) =
			add (rec_edge edge0) (rec_edge edge1)

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
	module TO_BRYANT_MODELE =
	struct
		module M = G1

		type xedge  = BryantB.GroBdd.G0.edge'
		type xedge' = Bitv.t
		type extra  = BryantB.GroBdd.G1.manager

		let o3_xedge = BryantB.GroBdd.G0.o3b_edge'

		let shift n ((b, (x, y)), i) = ((b, (x+n, y)), i)

		let isP = function NniTypes.P(false, None) -> true | _ -> false

		let map_edge _ apply ((b, l), i) =
			if i = Utils.Leaf () && List.for_all isP l
			then ((b, (List.length l, 0)), Utils.Leaf())
			else
			(
				let rec aux carry = function
					| [] -> assert false
					| head::tail as liste -> if isP head
						then (aux (1+carry) tail)
						else (carry, liste)
				in 
				let n, l = aux 0 l in
				shift n (apply ((b, l), i))
			)

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

		let shift n ((b, (x, y)), i) = ((b, (x+n, y)), i)

		let isP = function NniTypes.P(false, None) -> true | _ -> false

		let map_edge _ apply (((b, l), i) as edge) =
			if i = Utils.Leaf () && List.for_all isP l
			then ((List.length l, 0), Utils.Leaf b)
			else (apply edge)

		let push bryant = ZddB.GroBdd.G1.push bryant
	end

	module TO_ZDD = BinUbdagTC.EXPORT(TO_BRYANT_MODELE)

	module OF_CP_MODELE =
	struct
		module M = CpB.GroBdd.G0

		type extra  = G1.manager
		type xnode  = G0.edge'
		type xnode' = Bitv.t
		type xedge  = G0.edge'

		let o3_xnode = G0.o3b_edge'

		let map_edge (b, u) =
			let funmap = function
				| CpTypes.S -> NniTypes.S false
				| CpTypes.P -> NniTypes.P (false, None)
			in
			(b, List.map funmap u)

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
		let man = OF_CP.newman (CpB.GroBdd.G1.export pure_cp) pure_cpx in
		let map = OF_CP.rec_edge man in
		(man, (pure_cpx, List.map map edges))

end

module TACX =
struct

	module M0 =
	struct
		type leaf = unit
		type edge = NniTypes.edge_state
		type node = TacxTypes.tag

		let strdump_leaf = StrDump.unit
		let strdump_edge = NniBDumpLoad.strdump_edge
		let strdump_node = TacxTypes.strdump_tag

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = NniBDumpLoad.(bindump_edge, binload_edge)
		let o3s_node = TacxTypes.o3s_tag

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node NniBDumpLoad.(bindump_tacx, binload_tacx) o3s_leaf o3s_ident

		let __check_reverse__ = false

	end

	module M1 =
	struct
		module M = M0
		
		let push_node = NniBGops.solve_tacx

		let compose = NniBGops.compose


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
			"[label = \""^(NniBDumpLoad.strdump_edge edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node = TacxTypes.strdump_tag
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile = TO_DOT.dotfile

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
		let man = COMPILE.makeman tacx grobdd hsize in
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

		let map_edge (b, u) =
			let funmap = function
				| CpTypes.S -> NniTypes.S false
				| CpTypes.P -> NniTypes.P (false, None)
			in
			(b, List.map funmap u)

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

end

let newman = GroBdd.newman

let pass = NniTypes.P(false, None)

let make_const b n = ((b, MyList.ntimes pass n), Utils.Leaf ())

let make_ident man b n = GroBdd.push man ((), make_const b n, make_const (not b) n)

let arity ((_, l), _) = List.length l

let push_pass ((b, l), i) = ((b, (pass::l)), i)

let neg ((b, l), i) = ((not b, l), i)
let cneg b' ((b, l), i) = ((b' <> b, l), i)

let is_root = function
	| ((b, _), Utils.Leaf ()) -> Some b
	| _ -> None 

let get_root b ((_, l), _) = make_const b (List.length l)

let ( *! ) man x y = man.TACX.push (TacxTypes.Cons, x, y)
let ( &! ) man x y = man.TACX.push (TacxTypes.And , x, y)
and ( ^! ) man x y = man.TACX.push (TacxTypes.Xor , x, y)

(*
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
				assert(List.for_all (function NniTypes.P -> true | _ -> false) l);
				Utils.MEdge ((b, (List.length l, 0)), Utils.Leaf())
			)
			| Utils.Node n ->
			(
				let rec aux carry = function
					| [] -> assert false
					| head::tail as liste -> match head with
						| NniTypes.P -> aux (1+carry) tail
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
				assert(List.for_all (function NniTypes.P -> true | _ -> false) l);
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
*)
