(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)
open Extra
open O3Extra

let o3s_leaf = BinO3.bool

let o3s_next' o3s_ident = Utils.o3s_gnode o3s_leaf o3s_ident

module GroBdd =
struct
	module M0 =
	struct
		type leaf = bool
		type edge = ZddTypes.edge_state
		type node = unit

		let strdump_leaf = StrDump.bool
		let strdump_edge = ZddBGops.strdump_edge
		let strdump_node = StrDump.unit

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_leaf = o3s_leaf
		let o3s_edge = ZddBGops.(bindump_edge, binload_edge)
		let o3s_node = BinO3.unit

		let o3s_next' = o3s_next'
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident =
			BinUbdag.default_o3s_node ZddBGops.(bindump_node, binload_node) o3s_leaf o3s_ident

		let __check_reverse__ = true

	end

	module M1 =
	struct
		module M = M0
		
		let arity     = ZddBGops.arity
		let compose   = ZddBGops.compose
		let push_node = ZddBGops.solve_node
		let pull_node = ZddBGops.node_pull
	end

	include BinUbdagTC.STDIO(M1)

	module TO_DOT_MODELE =
	struct
		module M = G0

		let string_of_leaf leaf = "L"^(StrUtil.string_of_bool leaf)

		let string_of_pos = function
			| None       -> "black"
			| Some false -> "red"
			| Some true  -> "green"

		let string_of_edge pos edge =
			"[label = \""^(ZddBGops.strdump_edge edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node _ = ""
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile = TO_DOT.dotfile

end

