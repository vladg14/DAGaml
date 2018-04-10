(* I) Header *)
(* Creates of a manager [man] to handle circuits with
		And, Xor, Not and Cons (i.e. shannon's operator). *)
let man = Cnax.CNAX.G1.newman();;
(* Instanciates the OPeratorS library of DAGaml with our
		manager *)
module OOPS = Cnax.OOPS(struct let cnax = man end);;
(* Opens the root module which defines operators:
	- And (&!)
	- Or  (|!)
	- Eq  (=!)
	- Xor (^!)
	- Not (neg)
*)
open OOPS.M0
(* Opens the Variable size Unsigned Integer library, which
		defines the following operators:
	- Addition (+/)
	- Equality (=/)
*)
open OOPS.VUInt


(* [1] heuristic : clique *)
let op1 = false
(* [2] heuristic : terminal is maximal *)
and op2 = false
(* [3] heuristic : [sufficient] ex opt seq with eq-neighbors *)
and op3 = true
(* [4] heuristic : [necessary] optimal components are maximal *)
and op4 = true
;;

(* [step] encodes one stage of suppression :
	1] Definition and Notations
	[step arity size alive component graph] = [alive', graph', cost, restrict]
	where [alive'] and [graph'] will become [alive] and [graph] at the next step
		- we denote n = [size] the number of nodes
		- we denote G = (V, E) an undirected graph with
			    V = {i \in [1, n] | [alive.(i) = true] }
			and E such that (i, j) \in E iff [Matrix.get (i, j) = true]
		- we denote X = {i \in [1, n] | [component.(i) = true] } the  component to suppress in G
		- we similarly denote G' = (V', E') the analogous of G relatively to [alive'] and [graph']
	2] Computation step
		- V' = V \ X, i.e.
			=> forall i, alive'.(i) = alive.(i) /\ not component.(i)
		- N = {j \in V | exists i \in X, (i, j) \in E}
			=> forall i, neighbors.(i) = component.(i) \/
				(alive.(i) /\ (exists j, component.(j) /\ (i, j) \in E))
		- E' = E + N^2 - (X x V+V x X) = V'^2 /\ (E \/ (N^2))
			=> forall i j, (i, j) \in E' =
				alive'.(i) /\ alive'.(j) /\ ( (i, j) \in E \/ (i, j) \in N^2 )
		- [cost] = 2^|N|
			=> [cost = exp_card neighbors]
		- [restrict] =
				if G is a clique then V = X
					(we may prove this constraint to be necessary for optimal sequences)
			=> clique = forall i j, (i, j) \in V^2 => (i, j) \in E
				 nomore = forall i, not alive'.(i)
				 [restrict = clique => nomore]
*) 
let step
		(arity:int) (size:int)
		(* alive.(i) iff the i-th node is still active *)
		(alive:OOPS.edge array)
		(* {i | component.(i)} is the set of node to be suppressed *)
		(component:OOPS.edge array)
		(* [graph] represents the adjacency matrix at this stage *)
		(graph:OOPS.edge Matrix.matrix) =

	(* Computes the next generation of alive nodes *)
	let alive' = Array.map2 (fun x y -> x &! (neg y)) alive component in
	(* Computes the current set of neighbours of our component *)
	let neighbors = Array.init size
		(fun i -> component.(i) |! (alive.(i) &! (OOPS.init_or size
			(fun j -> component.(j) &! (Matrix.get graph (i, j)))))) in
	(* [valid] is true iff this stage has to be taken into account
		 i.e. is the component non-empty *)
	let valid = G0.array_or component in
	(* Computes the next generation adjacency matrix *)
	let graph' = Matrix.init (size, size) (fun (i, j) ->
		valid &! alive'.(i) &! alive'.(j) &!
		(Matrix.get graph (i, j) |! (neighbors.(i) &! neighbors.(j))))
	in
	(* zero_choice c x = if c then x else 0 *)
	(* if valid then 2^|neighbors| else 0 *)
	let cost = zero_choice valid (exp_card arity neighbors) in
	let restrict = cst true arity in
        (* [1] [necessary] if [graph] is a clique then suppress all nodes *)
	let restrict = if op1
		then (
			(* [nomore = true] iff [forall i, alive'.(i) = false] *)
			let nomore = G0.array_and (Array.map neg alive') in
			(* [clique = true] iff [graph] restricted to its lively nodes
					is a clique *)
			let clique =
				OOPS.init_and size (fun i -> (neg alive.(i)) |!
					OOPS.init_and size (fun j -> (neg alive.(j)) |!
						(Matrix.get graph (i, j))
					)
			) in
			restrict &! ((neg clique)|!nomore)
		) else restrict in
        (* [2] [sufficient] if neighbor = alive then suppress all nodes *)
	let restrict = if op2
		then (
			let cond = G0.array_and (Array.map2 (=!) alive neighbors) in
			let impl = G0.array_and (Array.map neg alive') in
			restrict &! ((neg cond)|! impl)
		) else restrict in
        (* [3] [sufficient] all nodes in the component have the same neighborhoud *)
	let restrict = if op3
		then (
			let restrict' = G0.init_and size
				(fun i -> (neg component.(i)) |! G0.init_and size
					(fun j -> (neg component.(j)) |! (Matrix.get graph (i, j)))) in
			restrict &! restrict'
		) else restrict in
        (* [4] [necessary] optimal components are maximal *)
	let restrict = if op4
		then (
			let restrict' = G0.init_and size
				(fun i -> (neg alive'.(i)) |! component.(i) |! (G0.init_or size
					(fun j -> neg neighbors.(j)))) in
			restrict &! restrict'
		) else restrict in
(*
	let restrict = if false
		then (
		) else restrict in
*)
	(alive', graph', cost, restrict)

let array_imp array =
	OOPS.init_and (Array.length array - 1) (fun i -> (neg array.(i)) |! array.(i+1))

let rev_array_imp array =
	OOPS.init_and (Array.length array - 1) (fun i -> (neg array.(i+1)) |! array.(i))

let make graphAA size arity vars =
	(* Decomposes vars in two :
			- [input] n^2 bits which encode the ordered partition matrix
			- [upper] n+1 bits which encode the overall cost of this decomposition
	*)
	let input = Array.init (size*size) (fun i -> vars.(i)) in
	let upper = {arity; array = Array.init (size+1) (fun i -> vars.((size*size)+i))} in
	(* Each row of [input] represents a component of the suppressing sequence.
		 Each column of [input] represents at which stage a node is suppressed *)
	let input = Matrix.matrix_of_array size size input in
	(* Thus each nodes is suppressed exactly once *)
	let exact1 = OOPS.(Matrix.(input
		|> transpose
		|> array2_of_matrix
		|> Array.map array_exactly_one
		|> array_and
	)) in
	(* Allows the sequence to end with empty components,
			we set the cost of suppressing 0 nodes to 0 *)
	let early =	(input
		|> Matrix.array2_of_matrix
		|> Array.map OOPS.array_or
		|> rev_array_imp
	) in
	(* We create [size] stages of suppression, accumulating costs and constraints *)
	let rec aux cost_carry cst_carry alive graphAA = function
		| [] -> (list_add cost_carry, OOPS.list_and cst_carry, alive, graphAA)
		| input :: inputs -> (
			let alive, graphAA, cost, cst = step arity size alive input graphAA in
			aux (cost::cost_carry) (cst::cst_carry) alive graphAA inputs
		)
	in
	let cost, cst, alive, graphAA = aux
		[] []
		(Array.make size (cst true arity))
		(Matrix.map (fun bool -> cst bool arity) graphAA)
		(Matrix.array2_of_matrix input |> Array.to_list)
	in
	(* [forall i, alive.(i) = false] *)
	let used = alive |> G0.array_neg |> G0.array_and in
	(* [forall i j, graphAA.(i, j) = false] *)
	let empty = Matrix.array_of_matrix graphAA |> G0.array_neg |> G0.array_and in
	(* [upper <= 2^n *)
	let upper_le_2n = upper <=/ (uint_of_int arity (1 lsl size)) in
	(* [upper = sigma_i cost_i ] *)
	let cost_is_upper = cost =/ upper in
	(upper_le_2n &! empty &! cost_is_upper &! exact1 &! early &! used &! cst)

let main file_in file_out out_cnf: unit =
	let graphLA = file_in |> STree.loadfile |> List.hd |> GraphLA.strload_graph in
	let graphAA = (GraphAA.aa_of_la graphLA).GraphAA.edges in
	let (size, size') = Matrix.dim graphAA in
	assert(size' = size);
	print_string "size = "; print_int size; print_newline();
	let arity = size*size + (size+1) in
	let vars = OOPS.array_make_n_var arity in
	print_string "Building Circuit : start"; print_newline();
	let success = make graphAA size arity vars in
	print_string "Building Circuit : done!"; print_newline();
        (* [5] *)
	if out_cnf
	then
	(
		print_string "CNF export : start"; print_newline();
		let mcnf = Cnax.CNAX.export_cnf man success.arity [success.edge] in
		StrDumpCnf.dump_file file_out mcnf;
		print_string "CNF export : done!"; print_newline()
	) else (
		print_string "Verilog export : start"; print_newline();
		let mexpr = IoUtils.module_expr_of_cnax
			"icp"
			success.arity
			man
			[|success.edge|]
		in
		StrDumpVerilog.dump_file file_out mexpr;
		print_string "Verilog export : done!"; print_newline()
	)

let _ = main Sys.argv.(1) Sys.argv.(2)
	(	if Array.length Sys.argv <= 3
		then false
		else match Sys.argv.(3) with
		| "-cnf" -> true
		| "-v" -> false
		| _ -> assert false)
	 
