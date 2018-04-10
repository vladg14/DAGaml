(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open NniTypes
open Extra

let pretty_riuniq b =
	let iuniq_elem n =
		let rec aux carry = function
			| ([], []) -> (MyList.ntimes " " (n+1))@("+"::(List.rev carry))
			| ((IS _)::x', y::y') -> aux ((if y then "+" else " ")::carry) (x', y')
			| ((IP _)::x', y') -> aux (" "::carry) (x', y')
			| _ -> assert false
		in fun vec tail -> aux [] (tail, vec)
	in
	let rec aux n floor matrix = function
		| [] -> String.concat "\n" ((List.map (String.concat "") ((List.rev matrix)@[(if b then "-" else "+")::(List.rev floor)])))
		| head::tail -> match head with
			| IS b				-> aux (n+1) ((if b then "F" else "S")::floor) matrix tail
			| IP (b, None)		-> aux (n+1) ((if b then "A" else "U")::floor) matrix tail
			| IP (b, Some v)	-> aux (n+1) ((if b then "N" else "P")::floor) ((iuniq_elem n v tail)::matrix) tail
	in
	aux 0 [] []

let pretty_edge (b, u) = pretty_riuniq b (xiuniq_of_xuniq u)

let strdump_invar = StrDump.(pair bool (option bitv_bool))
let bindump_invar = BinDump.(pair bool (option bitv))
let binload_invar i = BinLoad.(pair bool (option (bitv i)))

let strdump_uniq_elem = function
	| S b -> "S "^(StrDump.bool b)
	| P i -> "P "^(strdump_invar i)

let bindump_uniq_elem stream = function
	| S b	-> false::b::stream
	| P i	-> true::(bindump_invar i stream)

let binload_uniq_elem i = BinLoad.choice
(* S *)	(fun stream ->
	let b, stream = BinLoad.bool stream in
	(S b, (i+1), stream)
		)
(* P *) (fun stream ->
	let invar, stream = binload_invar i stream in
	(P invar, i , stream)
		)

let strdump_uniq = StrDump.list strdump_uniq_elem
let bindump_uniq uniq stream = 
	let rec fold stream = function
		| [] -> stream
		| head::tail -> fold (true::(bindump_uniq_elem stream head)) tail
	in
	fold (false::stream) uniq

let binload_uniq = 
	let rec fold i carry = BinLoad.choice
		(fun stream -> carry, stream)
		(fun stream ->
			let e, i, stream = binload_uniq_elem i stream in
			fold i (e::carry) stream)
	in fold 0 []


let bindump_pair_elem carry = function
	| SS b					-> false::b::carry
	| SP invar				-> true::false::true::(bindump_invar invar carry)
	| PS invar				-> true::false::false::(bindump_invar invar carry)
	| PP (invarX, invarY)	-> true::true ::false::(bindump_invar invarX (bindump_invar invarY carry))

let bindump_pair = (let rec fold carry = function
	| [] -> carry
	| head::tail -> fold (bindump_pair_elem carry head) tail in fold (true::true::true::[]))

let binload_pair_elem i j = function
	| false::b::stream				-> (* SS *)
		(i+1, j+1, SS b, stream)
	| true::false::true::stream		-> (* SP *)
		let invar, stream = binload_invar j stream in
		(i+1, j, SP invar, stream)
	| true::false::false::stream	-> (* PS *)
		let invar, stream = binload_invar i stream in
		(i, j+1, PS invar, stream)
	| true::true ::false::stream	-> (* PP *)
		let invarX, stream = binload_invar i stream in
		let invarY, stream = binload_invar j stream in
		(i, j, PP (invarX, invarY), stream)
	| _								->  assert false

let binload_pair = 
	let rec fold i j carry = function
		| [] -> carry
		| true::true ::true :: _ -> carry
		| stream ->
			let i, j, e, stream = binload_pair_elem i j stream in
			fold i j (e::carry) stream
	in fold 0 0 []

let strdump_edge = StrDump.(pair bool strdump_uniq)
let bindump_edge = BinDump.(pair bool bindump_uniq)
let binload_edge = BinLoad.(pair bool binload_uniq)

let bindump_node = BinDump.(trio unit bindump_edge bindump_edge)
let binload_node = BinLoad.(trio unit binload_edge binload_edge)

let bindump_tacx = BinDump.trio TacxTypes.bindump_tag bindump_edge bindump_edge
let binload_tacx = BinLoad.trio TacxTypes.binload_tag binload_edge binload_edge
