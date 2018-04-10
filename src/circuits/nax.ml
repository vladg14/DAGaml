open Extra
open O3Extra

type nax =
	| And
	| Xor

let strdump_nax = function
	| And      -> "And"
	| Xor      -> "Xor"

let bindump_nax nax stream = match nax with
	| And      -> false::stream
	| Xor      -> true ::stream

let binload_nax = function
	| false::stream -> And, stream
	| true ::stream -> Xor, stream
	| _ -> assert false

module NAX_MODELE =
struct
	module M =
	struct
		type leaf = int option (* [Some var] = [var] | [None] = false *)
		type edge = bool
		type node = nax

		let strdump_leaf = StrDump.(option int)
		let strdump_edge = StrDump.bool
		let strdump_node = strdump_nax

		let o3s_leaf = BinO3.(option int)
		let o3s_edge = BinO3.bool
		let o3s_node = (bindump_nax, binload_nax)

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
	let push_node (node, (neg0, next0), (neg1, next1)) = match node with
	| And -> (match next0 with
		| Utils.Leaf None -> (if neg0 then (* edge0 = true *) (neg1, Utils.MEdge next1) else (false, Utils.MEdge(Utils.Leaf None)))
		| _ -> match next1 with
			| Utils.Leaf None -> (if neg1 then  (* edge1 = true *) (neg0, Utils.MEdge next0) else (false, Utils.MEdge(Utils.Leaf None)))
			| _ -> if next0 = next1
				then (if neg0 = neg1 then (neg0, Utils.MEdge next0) else (false, Utils.MEdge (Utils.Leaf None)))
				else (false, Utils.MNode (And, (neg0, next0), (neg1, next1))))
	| Xor -> (match next0 with
		| Utils.Leaf None -> (neg0<>neg1, Utils.MEdge next1)
		| _ -> match next1 with
			| Utils.Leaf None -> (neg0<>neg1, Utils.MEdge next0)
			| _ -> if next0 = next1
				then (neg0<>neg1, Utils.MEdge(Utils.Leaf None))
				else (false, Utils.MNode (Xor, (neg0, next0), (neg1, next1))))
	let compose negx (negy, nexty) = (negx<>negy, nexty)
end

module NAX = BinUbdagT.MODULE(NAX_MODELE)

module TO_IFormula =
struct
	type carry = {
		ninputs : int;
		mutable wire_cnt : int;
		mutable assign   : int Expr.expr list;
	}
	module MODELE = 
	struct
		module M = NAX.G

		type extra = carry
		type xnode = int
		type xedge = int Expr.expr

		let map_edge extra (neg, node) = Expr.(PUop ((if neg then PNot else PNop),
			(match node with
				| Utils.Node node -> PVar (node())
				| Utils.Leaf leaf -> match leaf with
						| Some var -> assert(0 <= var && var < extra.ninputs); PVar var
						| None -> PCst false
			)))

		let map_node extra (node, edge0, edge1) =
			let expr0 = map_edge extra edge0
			and expr1 = map_edge extra edge1 in
			let wire = extra.wire_cnt in
			extra.wire_cnt <- extra.wire_cnt + 1;
			let oper = Expr.(match node with And -> PAnd | Xor -> PXor) in
			extra.assign <- Expr.(PBop (oper, expr0, expr1))::extra.assign;
			wire
	end

	module MODULE = BinUbdag.EXPORT_NOC(MODELE)

	let export (man:NAX.G.manager) (ninputs : int) (outputs : NAX.G.edge' array) =
		let extra = {
			ninputs;
			assign = [];
			wire_cnt = ninputs;
		} in
		let eman = MODULE.newman man extra in
		let map_edge = MODULE.rec_edge eman in
		let apply edge =
			let expr = map_edge edge in
			extra.assign <- expr :: extra.assign;
			let wire = extra.wire_cnt in
			extra.wire_cnt <- extra.wire_cnt + 1;
			wire
		in
		let ioutput = Array.map apply outputs in
		let iinput = Array.init ninputs (fun x -> x) in
		Expr.{iinput; ioutput; iassign = Array.of_list (List.rev extra.assign)}
end
