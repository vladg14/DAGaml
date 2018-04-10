open CnfTypes

type token =
	| Str of string
	| Int of int

let my_lexer stream : token Stream.t =
	let is_space = function ' ' | '\t' | '\n' -> true | _ -> false in
	let rec rmline () =
		try match Stream.next stream with
			| '\n' -> ()
			|  _   -> rmline ()
		with _ -> ()
	in
	let rec spaces () = match Stream.peek stream with
	| None -> ()
	| Some head -> if is_space head then (Stream.junk stream; spaces ()) else ()
	in
	let word () =
		let rec aux s = match Stream.peek stream with
			| None -> StrUtil.implode (List.rev s)
			| Some c ->
			( if is_space c
				then (StrUtil.implode (List.rev s))
				else (Stream.junk stream; aux (c::s))
			)
		in
		let w = aux [] in
		try Int(int_of_string w)
		with _ -> Str w
	in
	let mrmc = ref true in
	let rec rmc () = match Stream.peek stream with
		| Some 'c' -> rmline(); rmc ()
		| _ -> ()
	in
	let rec aux x : token option=
		if !mrmc then (mrmc:=false; rmc());
		spaces ();
		match Stream.peek stream with
		| None -> None
		| Some head ->
		(
			assert(not(is_space head));
			Some(word())
		)
	in
	Stream.from aux

let pstring x = print_string x; print_string " "; flush stdout
(* let pstring x = () *)
let pchar x = print_char x; print_char ' '; flush stdout
(* let pchar x = () *)

let parse_clause stream =
	let rec aux carry = match Stream.next stream with
	| Int x -> if x = 0
		then List.rev carry
		else aux ((if x < 0 then (true, -x-1) else (false, x-1))::carry)
	| _ -> assert false
	in aux []

let parse_str stream = match Stream.next stream with
	| Str x -> x
	| _ -> assert false


let parse_int stream = match Stream.next stream with
	| Int x -> x
	| _ -> assert false


let parse_module stream =
	let rec aux nclause carry =
		if nclause = 0
		then (List.rev carry)
		else aux (nclause-1) ((parse_clause stream)::carry)
	in
	assert(parse_str stream = "p");
	assert(parse_str stream = "cnf");
	let input = parse_int stream in
	let nbclauses = parse_int stream in
	let clauses = aux nbclauses [] in
	{input; clauses}


let load stream = stream |> my_lexer |> parse_module

let load_file target =
	let file = open_in target in
	let cnf = load(Stream.of_channel file) in
	close_in file;
	cnf
