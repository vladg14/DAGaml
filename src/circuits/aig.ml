open Extra
open O3Extra

module AIG_MODELE =
struct
	module M =
	struct
		type leaf = int option (* [Some var] = [var] | [None] = false *)
		type edge = bool
		type node = unit

		let strdump_leaf = StrDump.(option int)
		let strdump_edge = StrDump.bool
		let strdump_node = StrDump.unit

		let o3s_leaf = BinO3.(option int)
		let o3s_edge = BinO3.bool
		let o3s_node = BinO3.unit

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_next' o3s_ident = Utils.o3s_gnode o3s_leaf o3s_ident
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident = BinUbdag.default_o3s_node
			(BinO3.trio o3s_node o3s_edge o3s_edge) o3s_leaf o3s_ident

		let __check_reverse__ = false
	end

	(* WIP *)
	let push_node node = (false, Utils.MNode node)
	let compose negx (negy, nexty) = (negx<>negy, nexty)
end

module AIG = BinUbdagT.MODULE(AIG_MODELE)
