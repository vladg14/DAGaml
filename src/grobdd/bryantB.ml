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
		type edge = BryantTypes.edge_state
		type node = unit

		let strdump_leaf = StrDump.unit
		let strdump_edge = BryantBGops.strdump_edge
		let strdump_node = StrDump.unit

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = BryantBGops.(bindump_edge, binload_edge)
		let o3s_node = BinO3.unit

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node BryantBGops.(bindump_node, binload_node) o3s_leaf o3s_ident

		let __check_reverse__ = true

	end

	module M1 =
	struct
		module M = M0

		let arity     = BryantBGops.arity
		let compose   = BryantBGops.compose
		let push_node = BryantBGops.solve_node
		let pull_node = BryantBGops.node_pull
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
			"[label = \""^(BryantBGops.pretty_edge edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node _ = ""
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile = TO_DOT.dotfile

	module AND_M =
	struct
		module M = G1
		
		let solver = BryantBGops.cons_and
	end

	module AND = BinUbdagTC.BINOP(AND_M)

	module XOR_M =
	struct
		module M = G1
		
		let solver = BryantBGops.cons_xor
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

end

let newman = GroBdd.newman

let make_const b n = ((b, (n, 0)), Utils.Leaf ())

let make_ident man b n = GroBdd.push man ((), make_const b n, make_const (not b) n)

let arity ((_, l), _) = List.length l

let push_pass ((b, (x, y)), i) = ((b, (x+1, y)), i)

let neg ((b, l), i) = ((not b, l), i)
let cneg b' ((b, l), i) = ((b' <> b, l), i)

let is_root = function
	| ((b, _), Utils.Leaf ()) -> Some b
	| _ -> None 

let get_root b ((_, (x, _)), _) = ((b, (x, 0)), Utils.Leaf())
