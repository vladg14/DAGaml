(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

type tag =
	| And
	| Cons
	| Xor

let bindump_tag tag stream = match tag with
	| And  -> false::stream
	| Cons -> true ::false::stream
	| Xor  -> true ::true ::stream

let binload_tag = function
	| false::stream	       -> And, stream
	| true ::false::stream -> Cons, stream
	| true ::true ::stream -> Xor , stream
	| _ -> assert false

let o3s_tag = (bindump_tag, binload_tag)

let strdump_tag = function
	| And  -> "A"
	| Cons -> "C"
	| Xor  -> "X"

let strload_tag = function
	| "A" -> And
	| "C" -> Cons
	| "X" -> Xor
	| _ -> assert false

type ttag =
	| TAnd  of bool * bool
	| TCons of bool
	| TXor

let bindump_ttag ttag stream = match ttag with
	| TAnd (b0, b1) -> false::b0   ::b1   ::stream
	| TCons b		-> true ::false::b    ::stream
	| TXor			-> true ::true ::stream

let binload_ttag = function
	| false::b0   ::b1   ::stream -> (TAnd (b0, b1)), stream
	| true ::false::b    ::stream -> (TCons b      ), stream
	| true ::true ::stream        -> (TXor         ), stream
	| _ -> assert false


let strdump_ttag ttag stream = match ttag with
	| TAnd (b0, b1) -> "A"::(GUtils.pm_of_bool b0)::(GUtils.pm_of_bool b1)::stream
	| TCons b		-> "C"::(GUtils.pm_of_bool b)::stream
	| TXor			-> "X"::stream

let strload_ttag = function
	| "A"::b0::b1::stream -> (TAnd (GUtils.bool_of_pm b0, GUtils.bool_of_pm b1), stream)
	| "C"::b::stream -> (TCons (GUtils.bool_of_pm b), stream)
	| "X"::stream -> (TXor, stream)
	| _ -> assert false
	
let default_eval_node eval_edge peval (tag, edge0, edge1) = match tag with
	| Cons -> (match peval with
		| [] -> assert false
		| head::peval -> match head with
			| None       ->
				Utils.MNode(Cons, eval_edge peval edge0, eval_edge peval edge1)
			| Some false ->
				Utils.MEdge(eval_edge peval edge0)
			| Some true  ->
				Utils.MEdge(eval_edge peval edge1)
	)
(* tag = And | Xor *)
	| tag  -> Utils.MNode(tag, eval_edge peval edge0, eval_edge peval edge1)
