(*module Bytes = String*)

let char_of_bool = function
	| true	-> '1'
	| false	-> '0'
let bool_of_char = function
	| '1'	-> true
	| '0'	-> false
	| _		-> failwith "[tools/strUtil] bool parsing failure - 7"
let string_of_bool = function
	| true	-> "1"
	| false	-> "0"
let pretty_of_bool = function
	| true	-> "1"
	| false	-> "."
let bool_of_string = function
	| "1"	-> true
	| "0"	-> false
	| _		-> failwith "[tools/strUtil] bool parsing failure - 14"
let print_bool = function
	| true	-> print_string "1"
	| false	-> print_string "0"

let char_0 = Char.code '0'
let char_A = Char.code 'A'
let char_a = Char.code 'a'

let explode s =
	let rec exp i l =
		if i < 0 then l else exp (i - 1) (s.[i] :: l)
	in exp (String.length s - 1) []

let implode l =
	let res = Bytes.create (List.length l) in
	let rec imp i = function
		| [] -> res
		| c :: l -> Bytes.set res i c; imp (i + 1) l
	in imp 0 l

let catmap s f l = String.concat s (List.map f l)

let index c s =
	try	 Some (String.index s c)
	with Not_found -> None

let split c s =
	let rec aux s = match index c s with
		| Some i ->	(
			let l = String.sub s 0 i
			and r = String.sub s (i+1) ((String.length s)-(i+1)) in
			l::(aux r)
					)
		| None -> [s]
	in aux s

let ntimes s n = String.concat "" (MyList.ntimes s n)
