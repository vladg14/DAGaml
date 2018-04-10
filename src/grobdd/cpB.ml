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
		type edge = CpTypes.edge_state
		type node = unit

		let strdump_leaf = StrDump.unit
		let strdump_elem = CpTypes.(function P -> "P" | S -> "S")
		let strdump_edge = StrDump.(pair bool (list strdump_elem))
		let strdump_node = StrDump.unit

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = CpBDumpLoad.(bindump_edge, binload_edge)
		let o3s_node = BinO3.unit

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node CpBDumpLoad.(bindump_node, binload_node) o3s_leaf o3s_ident

		let __check_reverse__ = false

	end

	module M1 =
	struct
		module M = M0

		let arity     = CpBGops.arity
		let compose   = CpBGops.compose
		let push_node = CpBGops.solve_cons
		let pull_node = CpBGops.node_pull
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
			"[label = \""^(CpBDumpLoad.strdump_edge edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node _ = ""
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile = TO_DOT.dotfile

	module AND_M =
	struct
		module M = G1
		
		let solver node =
			let edge, merge = CpBGops.solve_and node in
			(
				edge,
				match merge with
					| Utils.MEdge next -> Utils.M3Edge next
					| Utils.MNode node -> Utils.M3Node node
			)

	end

	module AND = BinUbdagTC.BINOP(AND_M)

	module XOR_M =
	struct
		module M = G1
		
		let solver node =
			let edge, merge = CpBGops.solve_xor node in
			(
				edge,
				match merge with
					| Utils.MEdge next -> Utils.M3Edge next
					| Utils.MNode node -> Utils.M3Node node
			)

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

	module PEVAL_MODELE =
	struct
		module M = G1

		type peval = bool option list

		let o3s_peval = BinO3.bool_option_list

		type next = peval option * M.G.ident

		type next' = next M.M.M.next'
		type edge' = next M.M.M.edge'
		type node' = next M.M.M.node'

		let eval_edge : peval -> edge' -> edge' = CpBGops.assign 
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

	module CntSat_MODELE =
	struct
		module M = G0

		type xnode  = BigInt.big_int * BigInt.big_int
		type xnode' = xnode
		type xedge  = BigInt.big_int
		type extra  = unit

		let dump_xnode x = x
		let load_xnode x = x

		type next' = (unit -> xnode) M.M.next'
		type edge' = (unit -> xnode) M.M.edge'
		type node' = (unit -> xnode) M.M.node'

		let cswap b (x, y) = if b then (y, x) else (x, y)
		let add (x, y) (x', y') = (BigInt.(+) x x', BigInt.(+) y y')
		let shift n (x, y) = (BigInt.shift_left x n, BigInt.shift_left y n)
		let p = function CpTypes.P -> true | _ -> false

		let rec_next = function
			| Utils.Leaf () -> (BigInt.zero, BigInt.unit)
			| Utils.Node node -> node()

		let rec_edge ((b, l), next) =
			cswap b (shift (MyList.count p l) (rec_next next))

		let map_node () ((), edge0, edge1) =
			add (rec_edge edge0) (rec_edge edge1)

		let map_edge () edge = rec_edge edge |> fst
	end

	module CntSat = BinUbdag.EXPORT_NOC(CntSat_MODELE)

	let cntsat grobdd edges =
		let man = CntSat.newman (G1.export grobdd) () in
		let map = CntSat.rec_edge man in
		(man, List.map map edges)

	module AllSat_MODELE =
	struct
		module M = G0

		type xnode  = (bool option list list) * (bool option list list)
		type xnode' = xnode
		type xedge  = (bool option list list)
		type extra  = unit

		let o3_xnode = O3.id

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

		let rec_next : (unit -> xnode) M0.next' -> xnode = function
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

	module TO_BRYANT_MODELE =
	struct
		module M = G1

		type xedge  = BryantB.GroBdd.G0.edge'
		type xedge' = Bitv.t
		type extra  = BryantB.GroBdd.G1.manager

		let o3_xedge = BryantB.GroBdd.G0.o3b_edge'

		let shift n ((b, (x, y)), i) = ((b, (x+n, y)), i)

		let map_edge _ apply ((b, l), i) = match i with
			| Utils.Leaf () ->
			(
				assert(List.for_all (function CpTypes.P -> true | _ -> false) l);
				((b, (List.length l, 0)), Utils.Leaf())
			)
			| Utils.Node node ->
			(
				let rec aux carry = function
					| [] -> assert false
					| head::tail as liste -> match head with
						| CpTypes.P -> aux (1+carry) tail
						| _			-> carry, liste
				in 
				let n, l = aux 0 l in
				shift n (apply ((b, l), Utils.Node node))
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

		let map_edge _ apply (((b, l), i) as edge) =
			if i = Utils.Leaf ()
			then
			(
				assert(List.for_all (function CpTypes.P -> true | _ -> false) l);
				((List.length l, 0), Utils.Leaf b)
			)
			else (apply edge)

		let push bryant = ZddB.GroBdd.G1.push bryant
	end

	module TO_ZDD = BinUbdagTC.EXPORT(TO_ZDD_MODELE)
end

module TACX =
struct

	module M0 =
	struct
		type leaf = unit
		type edge = CpTypes.edge_state
		type node = TacxTypes.tag

		let strdump_leaf = StrDump.unit
		let strdump_elem = CpTypes.(function P -> "P" | S -> "S")
		let strdump_edge = StrDump.(pair bool (list strdump_elem))
		let strdump_node = TacxTypes.strdump_tag

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = CpBDumpLoad.(bindump_edge, binload_edge)
		let o3s_node = TacxTypes.o3s_tag

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node CpBDumpLoad.(bindump_tacx, binload_tacx) o3s_leaf o3s_ident

		let __check_reverse__ = false

	end

	module M1 =
	struct
		module M = M0
		
		let push_node = CpBGops.solve_tacx

		let compose = CpBGops.compose


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
			"[label = \""^(CpBDumpLoad.strdump_edge edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node node = "[label = \""^(TacxTypes.strdump_tag node)^"\" ];"
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

	module CP_TO_CPB_MODELE =
	struct
		type xnode = G0.edge'
		type xedge = G0.edge'
		type cons = xedge -> xedge -> xedge
		type extra = G1.manager

		let do_leaf _ () : xedge = ((false, []), Utils.Leaf())
		let do_node manB = CpGops.(binload_tacx >> tacx_split >> (fun (tag, edgeX, edgeY) ->
			Utils.MNode (fun nodeX nodeY ->
				G1.push manB (tag, M1.compose edgeX nodeX, M1.compose edgeY nodeY))))

		let do_edge _ e = M1.compose e
			
	end

	module CP_TO_CPB = Cp.TACX.NODE_VISITOR(CP_TO_CPB_MODELE)

	let cp_to_cpb manA edges =
		let manB = G1.newman () in
		let evaman, mapcalc = CP_TO_CPB.newman manA manB in
		(evaman, (manB, List.map mapcalc edges))
		

end

let newman = GroBdd.newman

let make_const b n = ((b, MyList.ntimes CpTypes.P n), Utils.Leaf ())

let make_ident man b n = GroBdd.push man ((), make_const b n, make_const (not b) n)

let arity = CpGops.arity_edge

let push_pass ((b, l), i) = ((b, CpTypes.P::l), i)

let neg ((b, l), i) = ((not b, l), i)
let cneg b' ((b, l), i) = ((b' <> b, l), i)

let is_root = function
	| ((b, _), Utils.Leaf ()) -> Some b
	| _ -> None 

let get_root b ((_, l), _) = ((b, MyList.ntimes CpTypes.P (List.length l)), Utils.Leaf ())

let ( *! ) man x y = man.TACX.push (TacxTypes.Cons, x, y)
let ( &! ) man x y = man.TACX.push (TacxTypes.And , x, y)
and ( ^! ) man x y = man.TACX.push (TacxTypes.Xor , x, y)

module OOPS(M0 : sig val tacx : TACX.G1.manager end) =
struct
	module M0 =
	struct
		type edge = TACX.G0.edge'
		let edge_of_blist blist =
			(false, List.map CpTypes.(function false -> P | true -> S) blist)
		let ( ->> ) blist = TACX.M1.compose (edge_of_blist blist)
		let arity  = CpGops.arity_edge
		(* where true represent significant variables and false non significant ones *)
		let    neg = CpGops.neg
		let push tag x y = TACX.G1.push M0.tacx (tag, x, y)
		let ( *! ) = push TacxTypes.Cons
		let ( &! ) = push TacxTypes.And
		let ( ^! ) = push TacxTypes.Xor
		let ( |! ) x y = neg((neg x) &! (neg y))
		(*let ( ^! ) x y = ((neg x) &! y) |! (x &! (neg y))*)
		let ( =! ) x y = neg(x ^! y)

		let cst = make_const
		let to_bool _ = None
	end

	include GOops.MODULE(M0)

	module SUInt = SUInt.MODULE(M0)
	module VUInt = VUInt.MODULE(M0)

end
