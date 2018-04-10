(* All Right Reserved

   Copyright (c) 2017 Joan Thibault & Lucas Fenouillet
*)

open GraphLA
open Extra

module OOPS = OBool.OOPS
open OOPS.M0
open OOPS.VUInt

exception NoProjection;;

let proj_edge_array array =
	try (
			Some(array |> Array.to_list
			||> (fun x -> match to_bool x with Some b -> b | None -> raise NoProjection))
	) with NoProjection -> None

let print_uint ?name uint =
	match proj_edge_array uint.array with
	| Some liste ->
	(
		(match name with Some name -> print_string name; print_newline() | None -> ());
		StrUtil.print_stream liste; print_newline();
		print_int (liste |> Tools.int_of_bin); print_newline()
	)
	| None -> ()

let print_barray ?name barray =
	match proj_edge_array barray with
	| Some liste ->
	(
		(match name with Some name -> print_string name | None -> ());
		StrUtil.print_stream liste; print_newline()
	)
	| None -> ()
	

let step
		(arity:int) (size:int)
		(alive:OOPS.edge array) (component:OOPS.edge array)
		(graph:OOPS.edge Matrix.matrix)
		(cost:uint) =
	print_barray ~name:"alive     : " alive;
	print_barray ~name:"component : " component;
	let alive' = Array.map2 (fun x y -> x &! (neg y)) alive component in
	print_barray ~name:"alive'    : " alive';
	let voisins = Array.init size
		(fun i -> component.(i) |! (alive.(i) &! (OOPS.init_or size
			(fun j -> component.(j) &! (Matrix.get graph (i, j)))))) in
	print_barray ~name:"voisins   : " voisins;
	let stricts = Array.map2 (&!) alive' voisins in
	print_barray ~name:"stricts   : " stricts;
	let graph' = Matrix.init (size, size) (fun (i, j) ->
		(Matrix.get graph (i, j) &! alive'.(i) &! alive'.(j)) |!
		(stricts.(i) &! stricts.(j)))
	in
	let ecard = exp_card arity voisins in
	print_uint ~name:"ecard" ecard;
	let mycost = zero_choice (G0.array_or component) ecard in
	print_uint ~name:"mycost" mycost;
	let cost' = cost +/ mycost in
	(alive', graph', cost')

let array_imp array =
	OOPS.init_and (Array.length array - 1) (fun i -> (neg array.(i)) |! array.(i+1))

let rev_array_imp array =
	OOPS.init_and (Array.length array - 1) (fun i -> (neg array.(i+1)) |! array.(i))

let make graphAA size arity vars =
	let graphAA = ref(Matrix.map (fun bool -> cst bool arity) graphAA) in
	let input = Array.init (size*size) (fun i -> vars.(i)) in
	let upper = {arity; array = Array.init (size+1) (fun i -> vars.((size*size)+i))} in
	print_uint ~name:"upper" upper;
	let input = Matrix.matrix_of_array size size input in
	let exact1 = OOPS.(Matrix.(input
		|> transpose
		|> array2_of_matrix
		|> Array.map array_exactly_one
		|> array_and
	)) in
	let early =	(input
		|> Matrix.array2_of_matrix
		|> Array.map OOPS.array_or
		|> rev_array_imp
	) in
	let alive = ref (Array.make size (cst true arity)) in
	let cost  = ref {arity; array = [||]} in
	let input = Matrix.array2_of_matrix input in
	for i = 0 to (size-1)
	do
		(* step arity size alive component graph cost *)
		let alive', graphAA', cost' = step
			(arity:int) (size:int)
			(!alive   : OOPS.edge array)
			(input.(i): OOPS.edge array)
			(!graphAA : OOPS.edge Matrix.matrix)
			(!cost    : uint) in
		alive   := alive'  ;
		graphAA := graphAA';
		cost    := cost'   ;
		print_uint ~name:"cost" cost';
	done;
	let dead = !alive |> G0.array_neg |> G0.array_and in
	((!cost <=/ upper) &! exact1 &! early &! dead)

let cost_S graph set =
(* Cost of S *)
(* Time Complexity O(n^2) *)
	let voisins = voisins graph set in
	let card_Nx = List.length voisins in
	let cost = BigInt.(shift_left unit card_Nx) in
	print_string "cost_S: "; BigInt.print cost; print_newline();
	cost

let function_S graph set =
	print_string "function_S:set: "; print_string StrDump.(list int set); print_newline();
(* apply S *)
(* Time Complexity O(n^2) *)
	let edges = Array.copy graph.edges in
	(* removes nodes in [set] from [graph] *)
	let nodes = minus graph.nodes set in

	(* removes obsolete edges *)
	List.iter (fun node -> edges.(node) <- []) set;
	
	let voisins = voisins_strict graph set in
	(* removes obsolete edges, add clique between nodes of the strict neighbourhood *)
	List.iter (fun node -> edges.(node) <- inter (union edges.(node) voisins) nodes) voisins;
	
	(cost_S graph set, {nodes; edges})

let function_SS seq g =
(* SS(g, [X0; X1; ...; Xk]) *)
	List.fold_left (fun (c, g) x ->
		let c', g' = function_S g x in (BigInt.(c+c'), g')) (BigInt.zero, g) seq

let parse_cnf_log ?size file_sol =
	let file = open_in file_sol in
	let first = input_line file in
	match first with
	| "SATISFIABLE"
	| "SAT" ->
	(
		let liste1 = input_line file |> StrUtil.explode in
		let liste2 = input_line file |> StrUtil.explode in
		close_in file;
		Some(List.map StrUtil.bool_of_char (liste1@liste2))
	)
	| "UNSATISFIABLE"
	| "UNSAT" -> close_in file; None
	| _ -> assert false

let replay_sol graphAA size arity (seq:bool list) =
	(* Pretty-Printing of the Solution *)
	let return = (
		print_string (seq ||> StrUtil.char_of_bool |> StrUtil.implode); print_newline();
		let vars = seq |> Array.of_list in
		let input = Array.init (size*size) (fun i -> vars.(i)) in
		let upper = Array.init (size+1) (fun i -> vars.((size*size)+i)) in
		let input = Matrix.matrix_of_array size size input in
		print_string (Matrix.pretty_bool_matrix input); print_newline();
		let matrix = Matrix.array2_of_matrix input in
		let sol = Array.init size (fun i -> Array.to_list (MyArray.indexes_true matrix.(i))) in
		let sol = List.filter (function [] -> false | _ -> true) (Array.to_list sol) in
		print_string "upper:"; print_newline();
		StrUtil.print_stream (upper |> Array.to_list); print_newline();
		let bound = upper |> Array.to_list |> Tools.int_of_bin in
		(sol, bound)
	) in
	(* OBool Building of the Solution *)
	let vars = seq ||> (fun b -> cst b arity) |> Array.of_list in
	let success = make graphAA size arity vars |> to_bool |> Tools.unop in
	print_string "success = "; StrUtil.print_bool success; print_newline();
	return

let parse_sol graphAA size file_sol =
	let arity = (size * size) + (size + 1) in
	match parse_cnf_log ~size:arity file_sol with
	| None -> None
	| Some seq -> Some(replay_sol graphAA size arity seq)

let main file_con file_sol : unit =
	let graphLA = GraphLA.strload_graph (List.hd(STree.loadfile file_con)) in
	let graphAA = (GraphAA.aa_of_la graphLA).GraphAA.edges in
	print_string (Matrix.pretty_bool_matrix graphAA); print_newline();
	let size = List.length graphLA.GraphLA.nodes in
	match parse_sol graphAA size file_sol with
	| Some(sol, bound) ->
	(
		print_string "SAT"; print_newline();
		print_string StrDump.(list (list int) sol); print_newline();
		print_string "bound : "; print_int bound; print_newline();
(*		let c, g = function_SS sol graphLA in
		print_string "value : "; BigInt.print c; print_newline(); *)
	)
	| None -> print_string "UNSAT"

let _ = main Sys.argv.(1) Sys.argv.(2);;

exit 0;;

