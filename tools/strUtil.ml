(*module Bytes = String*)

let char_of_bool = function
	| true	-> '1'
	| false	-> '0'
let bool_of_char = function
	| '1'	-> true
	| '0'	-> false
	| _		-> failwith "[tools/strUtil] bool parsing failure - 7"
let string_of_bool = function
	| true	-> "true"
	| false	-> "false"
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
let char_9 = Char.code '9'
let char_A = Char.code 'A'
let char_Z = Char.code 'Z'
let char_a = Char.code 'a'
let char_z = Char.code 'z'

let explode (s : string) : char list =
	let rec exp i l =
		if i < 0 then l else exp (i - 1) (s.[i] :: l)
	in exp (String.length s - 1) []

let implode (l : char list) : string =
	let res = Bytes.create (List.length l) in
	let rec imp i = function
		| [] -> res
		| c :: l -> Bytes.set res i c; imp (i + 1) l
	in imp 0 l

let catmap s f l = String.concat s (Extra.(l ||> f))

let index s c =
	try               Some (String.index s c)
	with Not_found -> None

let index_from s i c =
	try               Some(String.index_from s i c)
	with Not_found -> None

let split (c:char) (s:string) : string list =
	let n = String.length s in
	let rec aux carry i =
		if i >= n then (List.rev carry) else (
		match index_from s i c with
			| Some j ->
			(
				assert(i >= 0 && i < n);
				assert(j >= i && j < n);
				let str = String.sub s i (j-i) in
				aux (str::carry) (j+1)
			)
			| None ->
			(
				let str = String.sub s i (n-i) in
				List.rev (str::carry)
			)
		)
	in aux [] 0

let ntimes s n = String.concat "" (MyList.ntimes s n)

let print_stream stream = print_string(catmap "" Tools.string_of_bool stream)
