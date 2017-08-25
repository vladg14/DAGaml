(* All Right Reserved

   Copyright (c) 2017 Joan Thibault & Lucas Fenouillet
*)

(* Data Structure  *)
type graph = {nodes : int list; edges : int list array } (* G = (V, E) *)

(* Dump/Load *)

let strdump_graph graph = Tree.Node [
	StrTree.of_list StrTree.of_int graph.nodes;
	StrTree.of_array (StrTree.of_list StrTree.of_int) graph.edges
]

let strload_graph stree = function
	| Tree.Node [nodes; edges] ->
	{
		nodes = StrTree.to_list StrTree.to_int nodes;
		edges = StrTree.to_array (StrTree.to_list StrTree.to_int) edges
	}
	| _ -> assert false

(* Tools *)

let sorted : 'a list -> bool =
(* return true iff the input list is sorted *)
(* Time Complexity O(n) *)
	let rec aux x = function
		| [] -> true
		| y::t -> (x<y)&&(aux y t)
	in function
	| [] -> true
	| x::t -> (0<=x)&&(aux x t)

let sort_uniq : int list -> int list = List.sort_uniq Pervasives.compare

let inter lX lY =
(* return [lX] inter [lY] *)
(* Time Complexity O(nX+nY) *)
	let rec aux carry = function
		| ([], _) | (_, []) ->
			(List.rev carry)
		| (x::x', y::y') -> if x = y
			then (aux (x::carry) (x', y'))
			else (aux carry (if x < y
			then (x', y::y')
			else (x::x', y')))
	in aux [] (lX, lY)

let nointer lX lY =
(* returns (lX inter lY) = emptyset *)
(* Time Complexity O(nX+nY) *)
	let rec aux = function
		| ([], _) | (_, []) -> false
		| (x::x', y::y') ->
			(x<>y)&&(aux (if x < y
			then (x', y::y')
			else (x::x', y')))
	in aux (lX, lY)

let subset_of lX lY =
(* return true iff [lX] is a subset of [lY] *)
(* Time Complexity O(nX+nY) *)
	let rec aux = function
		| ([], _) -> true
		| (_, []) -> false
		| (x::x', y::y') -> if x = y
			then (aux (x', y'))
			else ((x<y)&&(aux (x', y::y')))
	in aux (lX, lY)

let union lX lY : _ list =
(* merges two already sorted list and returns a sorted list *)
(* removes duplicates *)
(* Time Complexity O(nX + nY) *)
	let rec aux carry = function
	| ([], l) | (l, []) -> (List.rev carry)@l
	| (x::x', y::y') -> if x = y
		then aux (x::carry) (x', y')
		else if x < y
		then aux (x::carry) (x', y::y')
		else aux (y::carry) (x::x', y')
	in
	aux [] (lX, lY)

let check graph =
(* returns true iff the graph [graph] is properly formed *)
(* Time Complexity O(n^2) *)
	let check_nodes nodes =
		(sorted nodes)&&(subset_of nodes graph.nodes)
	in
	(sorted graph.nodes)&&
	(
		let seq = Array.map (fun nodes -> nodes = []) graph.edges in
		List.iter (fun node -> seq.(node) <- true) graph.nodes;
		Array.for_all (fun x -> x) seq
	)&&
	(Array.for_all check_nodes graph.edges)

let reduce graph =
(* remove replicates *)
(* Time Complexity O(n^2.log(n)) *)
{
	nodes = List.sort_uniq Pervasives.compare graph.nodes;
	edges = Array.map (List.sort_uniq Pervasives.compare) graph.edges
}


let minus lX lY =
(* returns [lX] - [lY] *)
(* Time Complexity O(nX+nY) *)
	let rec aux carry = function
		| ([], _) -> []
		| (x, []) -> (List.rev carry)@x
		| (x::x', y::y') -> if x = y
			then (aux carry (x', y'))
			else if x < y
			then (aux (x::carry) (x', y::y'))
			else (aux carry (x', y'))
	in aux [] (lX, lY)

(* neighbourhood related tools *)

let voisins graph set =
(* returns the neighbourhood of [set] in [graph] *)
(* Time Complexity O( #edges( set - * ) + n ) = O(n^2)
	with #edges( set - * ) is the number of edges linked to set
*)
	let vect = Array.make (Array.length graph.edges) false in
	let set x = vect.(x) <- true in
	List.iter (fun x -> List.iter set graph.edges.(x)) graph.nodes;
	vect
		|> Array.mapi (fun i x -> if x then (Some i) else None)
		|> Array.to_list
		|> MyList.unop

let voisins_strict graph set =
(* returns the strict neighbourhood of [set] in [graph] *)
(* Time Complexity O(n^2) *)
	minus (voisins graph set) set
