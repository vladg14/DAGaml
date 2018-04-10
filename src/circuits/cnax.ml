open Extra
open O3Extra

type cnax =
	| Cons of int
	| And
	| Xor

let strdump_cnax = function
	| Cons var -> "Cons "^(string_of_int var)^""
	| And      -> "And"
	| Xor      -> "Xor"

let bindump_cnax cnax stream = match cnax with
	| Cons var -> false::(BinDump.int var stream)
	| And      -> true ::false::stream
	| Xor      -> true ::true ::stream

let binload_cnax = function
	| false::stream ->
	(
		let var, stream = BinLoad.int stream in
		(Cons var, stream)
	)
	| true ::false::stream -> (And, stream)
	| true ::true ::stream -> (Xor, stream)
	| _ -> assert false

module CNAX =
struct
	module M0 =
	struct
		type leaf = int option (* [Some var] = [var] | [None] = false *)
		type edge = bool
		type node = cnax

		let strdump_leaf = StrDump.(option int)
		let strdump_edge = StrDump.bool
		let strdump_node = strdump_cnax

		let o3s_leaf = BinO3.(option int)
		let o3s_edge = BinO3.bool
		let o3s_node = (bindump_cnax, binload_cnax)

		type 'i next' = (leaf, 'i) Utils.gnode
		type 'i edge' = edge * 'i next'
		type 'i node' = node * 'i edge' * 'i edge'

		let o3s_next' o3s_ident = Utils.o3s_gnode o3s_leaf o3s_ident
		let o3s_edge' o3s_ident = o3s_edge +* (o3s_next' o3s_ident)
		let o3s_node' o3s_ident = BinUbdag.default_o3s_node
			(BinO3.trio o3s_node o3s_edge o3s_edge) o3s_leaf o3s_ident

		let __check_reverse__ = false
	end

	module M1 =
	struct
		module M = M0
		(* WIP *)
		let push_node node = (false, Utils.MNode node)
		let compose negx (negy, nexty) = (negx<>negy, nexty)
	end

	include BinUbdagT.STDIO(M1)

	module TO_DOT_MODELE =
	struct
		module M = G0

		let string_of_leaf = function
			| Some var -> "X"^(string_of_int var)
			| None     -> "L0"

		let string_of_pos = function
			| None       -> "black"
			| Some false -> "red"
			| Some true  -> "green"

		let string_of_edge pos edge =
			"[label = \""^((function true -> "-" | false -> "+") edge)^"\"; color=\""^(string_of_pos pos)^"\"];"
		let string_of_node = function
		| Cons var -> "C(X"^(string_of_int var)^")"
		| And      -> "A"
		| Xor      -> "X"
	end

	module TO_DOT = BinUbdag.TO_DOT(TO_DOT_MODELE)

	let dotfile = TO_DOT.dotfile

	module TO_NAX_MODELE =
	struct
		module M = G0
		
		type extra  = Nax.NAX.manager
		type xnode  = Nax.NAX.G.edge'
		type xnode' = Bitv.t
		type xedge  = Nax.NAX.G.edge'

		let o3_xnode = Nax.NAX.G.o3b_edge'

		let rec_edge (neg, next) = match next with
			| Utils.Node node -> let (neg', node) = node () in (neg<>neg', node)
			| Utils.Leaf leaf -> (neg, Utils.Leaf leaf)
		let map_node extra ((node, edge0, edge1):(unit -> xnode) M.M.node') =
			let edge0 = rec_edge edge0
			and edge1 = rec_edge edge1 in
			let push = Nax.NAX.push extra in
			let negb (neg, node) = (not neg, node) in
			let (&!) edge0 edge1 = push(Nax.And, edge0, edge1) in
			let (|!) edge0 edge1 = negb((negb edge0)&!(negb edge1)) in
			match node with
			| Cons var ->
			(
				let leaf = Utils.Leaf(Some var) in
				let x0 = (true, leaf) and x1 = (false, leaf) in
				(x0 &! edge0)|!(x1 &! edge1)
			)
			| And			 -> push (Nax.And, edge0, edge1)
			| Xor			 -> push (Nax.Xor, edge0, edge1)
		let map_edge extra edge = rec_edge edge

	end

	module TO_NAX = BinUbdag.EXPORT(TO_NAX_MODELE)
	
	module RENAME_MODELE =
	struct
		module M = G0
		
		type extra  = G0.manager * (int array)
		type xnode  = G0.edge'
		type xedge  = G0.edge'

		let rec_edge rename (neg, next) = match next with
			| Utils.Node node -> let (neg', node) = node () in (neg<>neg', node)
			| Utils.Leaf leaf -> match leaf with
				| None -> (neg, Utils.Leaf None)
				| Some var -> (neg, Utils.Leaf(Some rename.(var)))
		let map_node (man, rename) ((node, edge0, edge1):(unit -> xnode) M.M.node') =
			let edge0' = rec_edge rename edge0
			and edge1' = rec_edge rename edge1 in
			let node' = match node with
				| Cons var -> Cons(rename.(var))
				| And      -> And
				| Xor      -> Xor in
			G1.push man (node', edge0', edge1')
		let map_edge (man, rename) edge = rec_edge rename edge

	end

	module RENAME = BinUbdag.EXPORT_NOC(RENAME_MODELE)

	module TO_CNF_MODELE =
	struct
		module M = G0

		type extra = {
			cst0 : int;
			mutable input : int;
			mutable clauses : (bool * int) list list;
		}
		type xnode = int
		type xedge = unit
		
		let rec_edge cst0 (neg, next) = (neg, (match next with
			| Utils.Node node -> node()
			| Utils.Leaf leaf -> (match leaf with Some var -> (var+1) | None -> cst0)))
		let map_node extra ((node, edge0, edge1):(unit -> xnode) M.M.node') =
			let edge0 = rec_edge extra.cst0 edge0
			and edge1 = rec_edge extra.cst0 edge1 in
			let root_var = extra.input + 1 in
			extra.input <- root_var;
			let root = (false, root_var) in
			let negb (neg, (node:int)) = (not neg, node) in
			let push clause = extra.clauses <- clause::extra.clauses in
			(match node with
			| Cons var ->
			(
				let var = (false, var+1) in
				push [var; root; negb edge0];
				push [var; negb root; edge0];
				push [negb var; root; negb edge1];
				push [negb var; negb root; edge1];
			)
			| And			 ->
			(
				push [root; negb edge0; negb edge1];
				push [negb root; edge0];
				push [negb root; edge1]
			)
			| Xor			 ->
			(
				push [root; edge0; negb edge1];
				push [root; negb edge0; edge1];
				push [negb root; edge0; edge1];
				push [negb root; negb edge0; negb edge1]
			));
			root_var
		let map_edge extra edge =
			(* do not merge these two lines, there is an implicit dependency *)
			let clause = [rec_edge extra.cst0 edge] in
			extra.clauses <- clause :: extra.clauses;
	end

	module TO_CNF = BinUbdag.EXPORT_NOC(TO_CNF_MODELE)

	let export_cnf cnax inputs edges =
		let extra = TO_CNF_MODELE.{
			cst0    = inputs+1;
			input   = inputs+1;
			clauses = [[(true, inputs+1)]];
		} in
		let tocnf = TO_CNF.newman cnax extra in
		List.iter (TO_CNF.rec_edge tocnf) edges;
		CnfTypes.{
			input   = extra.TO_CNF_MODELE.input;
			(* clauses = extra.TO_CNF_MODELE.clauses *)
			(* FIXME *)
			clauses = List.map (fun c ->
				List.map (fun (b, x) -> (b, x-1)) c
			) extra.TO_CNF_MODELE.clauses
		}

	module TEVAL_MODELE =
	struct
		module M = G0
		type extra = bool array
		type xnode = bool
		type xedge = bool
		
		let map_edge sigma (neg, next) = neg <> (match next with
			| Utils.Node node -> node()
			| Utils.Leaf None -> false
			| Utils.Leaf (Some var) -> sigma.(var) )
		let map_node sigma ((node, edge0, edge1):(unit -> xnode) M.M.node') =
			match node with
			| Cons var -> if sigma.(var)
				then (map_edge sigma edge1)
				else (map_edge sigma edge0)
			| And -> (map_edge sigma edge0)&&(map_edge sigma edge1)
			| Xor -> (map_edge sigma edge0)<>(map_edge sigma edge1)
	end

	module TEVAL = BinUbdag.EXPORT_NOC(TEVAL_MODELE)

	let total_eval cnax sigma edges =
		let teval = TEVAL.newman cnax sigma in
		Array.map (TEVAL.rec_edge teval) edges
		

end


module OOPS(M0 : sig val cnax : CNAX.G0.manager end) =
struct
	module M0 =
	struct
		type edge = {arity : int; edge : CNAX.G0.edge'}
		let rename_of_blist blist = Array.of_list (MyList.indexes (fun x -> x) blist)
		let ( ->> ) blist edge =
			let reman = CNAX.RENAME.newman M0.cnax (M0.cnax, (rename_of_blist blist)) in
			{arity = List.length blist; edge = CNAX.RENAME.rec_edge reman edge.edge}
		let arity edge = edge.arity
		(* where true represent significant variables and false non significant ones *)
		let   neg edge = {arity = edge.arity; edge = (not(fst(edge.edge)), (snd edge.edge))}
		let push tag x y = CNAX.G1.push M0.cnax (tag, x, y)
		let ( *! ) edge0 edge1 =
			assert(edge0.arity = edge1.arity);
			{arity = edge0.arity+1; edge = push (Cons edge0.arity) edge0.edge edge1.edge}
		let ( &! ) edge0 edge1 =
			assert(edge0.arity = edge1.arity);
			{arity = edge0.arity; edge = push And edge0.edge edge1.edge}
		let ( ^! ) edge0 edge1 =
			assert(edge0.arity = edge1.arity);
			{arity = edge0.arity; edge = push Xor edge0.edge edge1.edge}
		let ( |! ) x y = neg((neg x) &! (neg y))
		let ( =! ) x y = neg(x ^! y)

		let cst bool arity = {arity; edge = (bool, Utils.Leaf None)}
		let to_bool edge = None
	end

	include GOops.MODULE(M0)

	module SUInt = SUInt.MODULE(M0) (* fixed Size INTeger *)
	module VUInt = VUInt.MODULE(M0) (* Variable size INTeger *)

end

