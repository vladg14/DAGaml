let option dump stream = function
	| None -> false::stream
	| Some x -> true::(dump stream x)

let bool stream x = x :: stream
let bool_list stream x = x @ stream

let bitv stream x = bool_list stream (Bitv.L.to_bool_list x)


let sized_list dump =
	let rec aux stream = function
		| []			-> stream
		| head::tail	-> aux (dump head stream) tail
	in aux

let none_list dump liste stream =
	let rec aux stream = function
		| []			-> stream
		| head::tail	-> aux (dump (Some head) stream) tail

	in aux (dump None stream) liste

let int n stream =
	assert(n>=0);
	let rec aux c0 c1 = function
		| 0 -> c0@[true]@c1@stream
		| n -> aux (false::c0) ((n mod 2 = 1)::c1) (n/2)
	in aux [] [] n
