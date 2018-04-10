(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open IterExtra

let mp_of_bool = function
	| true	-> "-"
	| false	-> "+"
let mp_char_of_bool = function
	| true	-> '-'
	| false -> '+'
let pm_of_bool = function
	| true	-> "+"
	| false	-> "-"
let pm_char_of_bool = function
	| true	-> '+'
	| false -> '-'
let bool_of_pm = function
	| "+" -> true
	| "-" -> false
	| _ -> assert false
let bool_of_mp = function
	| "+" -> false
	| "-" -> true
	| _ -> assert false
let bool_of_pm_char = function
	| '+' -> true
	| '-' -> false
	| _ -> assert false
let bool_of_mp_char = function
	| '+' -> false
	| '-' -> true
	| _ -> assert false

let consensus (f : 'a * 'b -> 'c * (('d * 'e) option)) (x : 'a list) (y : 'b list) : (('c list) * (('d list) * ('e list))) =
	let rec aux carryC carry0 carry1 = function
		| ([], []) -> (List.rev carryC, (List.rev carry0, List.rev carry1))
		| (x::x', y::y') ->
		(
			let xC, opx01 = f (x, y) in
			let carry0, carry1 = match opx01 with
			| Some (x0, x1) -> (x0::carry0, x1::carry1)
			| None -> (carry0, carry1) in
			aux (xC::carryC) carry0 carry1 (x', y')
		)
		| [], _ | _, [] -> assert false
	in aux [] [] [] (x, y)

let consensus0 f x y =
	let rec aux carryC carry0 = function
		| ([], []) -> (List.rev carryC, List.rev carry0)
		| (x::x', y::y') ->
		(
			let xC, opx0 = f (x, y) in
			let carry0 = match opx0 with
			| Some x0 -> x0::carry0
			| None    ->     carry0 in
			aux (xC::carryC) carry0 (x', y')
		)
		| [], _ | _, [] -> assert false
	in aux [] [] (x, y)

let consensus2 f x y =
	let xy, xy' = List.split(List.map f (List.combine x y)) in
	let x', y' = List.split xy' in
	xy, (MyList.unop x'), (MyList.unop y')

let consensus3 f x y =
	let x', y' = List.split(List.map f (List.combine x y)) in
	(MyList.unop x', MyList.unop y')

let compose s lC lc =
	let rec aux carry = function
		| [] -> (function
			| [] -> (List.rev carry)
			| _ -> assert false
		)
		| x::x' -> if x = s
			then (function
				| []     -> assert false
				| y::y'  -> aux (y::carry) x' y'
			)
			else (fun y' -> aux (x::carry) x' y')
	in aux [] lC lc

type peval  = bool option list
type opeval = peval option

type quant = bool list
type opquant = quant option

let strdump_peval = StrDump.(list (option bool))
let strdump_opeval = StrDump.option strdump_peval

let reduce_opeval : opeval -> opeval = function
	| Some peval when List.exists Tools.isSome peval -> Some peval
	| _ -> None

let preduce_pnode = function
	| Utils.Leaf leaf -> Utils.Leaf leaf
	| Utils.Node (opeval, node) -> Utils.Node(reduce_opeval opeval, node)

let preduce_pedge (edge, pnode) = (edge, preduce_pnode pnode)

let gen_peval n =  (Iter.of_list [None; Some false; Some true]) $^ n;;

let compose_peval : peval -> peval -> peval = fun pevalC pevalc -> compose None pevalc pevalC

let compose_opeval opevalC opevalc = match (opevalC, opevalc) with
	| Some pevalC, Some pevalc -> Some(compose_peval pevalC pevalc)
	| Some peval, None
	| None, Some peval -> Some peval
	| None, None -> None
	
let default_eval_node eval_edge = function
	| [] -> assert false
	| head::peval -> match head with
		| None       -> fun ((), edge0, edge1) ->
			Utils.MNode((), eval_edge peval edge0, eval_edge peval edge1)
		| Some false -> fun((), edge0, _) ->
			Utils.MEdge(eval_edge peval edge0)
		| Some true  -> fun((), _, edge1) ->
			Utils.MEdge(eval_edge peval edge1)

