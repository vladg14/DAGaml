let unit : unit BinUtils.load = fun stream -> (), stream

let choice loadF loadT = function
	| b::stream -> (if b then loadT else loadF) stream
	| _			-> assert false

let option load = choice
	(fun stream -> None, stream)
	(fun stream ->
		let x, stream = load stream in
		Some x, stream)

let bitv size stream =
	let x, stream = MyList.hdtl_nth size stream in
	(Bitv.L.of_bool_list x), stream

let none_list load =
	let rec aux carry stream =
		let opelem, stream = load stream in
		match opelem with
		| None -> carry, stream
		| Some elem -> aux (elem::carry) stream
	in aux []

let while_list func init =
	let rec aux carry state stream =
		let op_elem_op_state, stream = func stream in
		match op_elem_op_state with
			| None -> (List.rev carry), stream
			| Some (elem, op_state) -> match op_state with
				| None -> (List.rev (elem::carry)), stream
				| Some state -> aux (elem::carry) state stream
	in aux [] init

(*
let sized_list load size next = 
	let rec aux size =
		let next = if size = 1 then next else aux (size-1) in
		fun carry -> load (fun x -> next (x::carry))
	in
	assert(size >= 0);
	if size = 0
	then next []
	else aux size []
*)
let sized_list (load : 'a BinUtils.load) (size : int) : 'a list BinUtils.load =
	(* print_string "sized_list"; print_newline(); StrUtil.print_stream stream; print_newline(); *)
	let rec aux carry stream = function
		| 0 -> carry, stream
		| n ->
		(
			let head, stream = load stream in
			aux (head::carry) stream (n-1)
		)
	in (fun stream -> aux [] stream size)

let list (load : 'a BinUtils.load) : 'a list BinUtils.load=
	let rec aux carry = function
		| false::stream	-> carry, stream
		| true::stream	->
			let elem, stream = load stream in
			aux (elem::carry) stream
		| _				-> assert false
	in aux []

let bool : bool BinUtils.load = function
	| b::stream -> b, stream
	| _			-> assert false

let unary : int BinUtils.load =
	let rec aux n = function
		| [] -> assert false
		| head::stream -> if head then (n, stream) else (aux (n+1) stream)
	in aux 0

let sized_int size : int BinUtils.load = fun stream ->
	(* print_string "sized_int"; print_newline(); StrUtil.print_stream stream; print_newline(); *)
	let l, stream = MyList.hdtl_nth size stream in
	let rec aux x = function
		| [] -> x
		| head::tail -> aux (x*2 + (if head then 1 else 0)) tail
	in (aux 0 l, stream)

let int : int BinUtils.load = fun stream ->
	let k, stream = unary stream in
	sized_int k stream

let pair (loadA : 'a BinUtils.load) (loadB : 'b BinUtils.load) : ('a * 'b) BinUtils.load = fun stream ->
	let a, stream = loadA stream in
	let b, stream = loadB stream in
	(a, b), stream

let trio loadA loadB loadC stream =
	let a, stream = loadA stream in
	let b, stream = loadB stream in
	let c, stream = loadC stream in
	(a, b, c), stream

let closure (load : 'a BinUtils.load) bitv : 'a =
	let objet, stream = bitv |> Bitv.L.to_bool_list |> load in
	assert(stream = []);
	objet

let list (load : 'a BinUtils.load) : 'a list BinUtils.load = fun stream ->
	let size, stream = int stream in
	sized_list load size stream

let array (load : 'a BinUtils.load) : 'a array BinUtils.load = fun stream ->
	let liste, stream = list load stream in
	(Array.of_list liste, stream)

let bool_option_list = none_list (function
	| b0::b1::stream ->
	((
		if b0
		then if b1
			then None
			else (Some None)
		else (Some(Some b1))
	), stream)
	| _ -> assert false)

let o3 = snd	
