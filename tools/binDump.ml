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


