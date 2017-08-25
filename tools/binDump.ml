let unit : unit BinUtils.dump = fun () stream -> stream

let option dump opx stream = match opx with
	| None -> false::stream
	| Some x -> true::(dump x stream)

let bool x stream = x :: stream
let bool_list x stream = x @ stream

let bitv x stream = bool_list (Bitv.L.to_bool_list x) stream


let sized_list dump liste stream =
	let rec aux stream = function
		| []			-> stream
		| head::tail	-> aux (dump head stream) tail
	in aux stream liste

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

let sized_int size n stream : bool list =
	assert(0 <= n && n <= (1 lsl size));
	let rec aux n stream = function
		| 0 -> assert(n=0); stream
		| i -> aux (n/2) ((n mod 2 = 1)::stream) (i-1)
	in
	(try (aux n stream size)
	with _ -> (
		print_int size;
		print_string " ";
		print_int n;
		print_newline();
		assert(false)
	))


let pair dumpA dumpB (a, b) stream = dumpA a (dumpB b stream)
let trio dumpA dumpB dumpC (a, b, c) stream = dumpA a (dumpB b (dumpC c stream))

let closure dump objet = dump objet [] |> Bitv.L.of_bool_list

let list dump liste stream = int (List.length liste) (sized_list dump liste stream)

let array dump vect stream = list dump (Array.to_list vect) stream

let bool_option_list = none_list (fun elem stream -> match elem with
	| Some (Some b) -> false::b    ::stream
	| Some  None    -> true ::false::stream
	| None          -> true ::true ::stream)

let o3 = fst
