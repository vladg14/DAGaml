let file_in = Sys.argv.(1)
and file_out = Sys.argv.(2);;

(* STEP 1: verilog parsing [START]*)

type token =
	| Str of string
	| Int of int

type module_cnf = {
	input  : int;
	clauses : (bool * int) list list;
}

type module_tacx = {
	tacx_name : string;
	tacx_input: string list;
	tacx_man  : Cp.TACX.manager;
	tacx_edges: (string * Cp.TACX.edge) list;
}

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
;;

let rec tee stream =
	let rec aux x =
		try
		(
			let char = Stream.next stream in
			print_char char; flush stdout;
			Some char
		)	
		with _ -> None
	in Stream.from aux

let pstring x = print_string x; print_string " "; flush stdout;;
(* let pstring x = ();; *)
let pchar x = print_char x; print_char ' '; flush stdout;;
(* let pchar x = ();; *)

let parse_clause stream =
	let rec aux carry = match Stream.next stream with
	| Int x -> if x = 0 then List.rev carry else aux ((if x < 0 then (true, -x-1) else (false, x-1))::carry)
	| _ -> assert false
	in aux []

let parse_str stream = match Stream.next stream with
	| Str x -> x
	| _ -> assert false
;;

let parse_int stream = match Stream.next stream with
	| Int x -> x
	| _ -> assert false
;;

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
;;

(* let tee stream = stream *)

let my_cnf = parse_module (my_lexer (tee(Stream.of_channel (open_in file_in))));;
(* STEP 1: CNF parsing [DONE] *)


(* STEP 2: from CNF to Cp.TACX *)

let upgrade mymodule =
	let man = Cp.TACX.newman() in
	let ( &! ) = Cp.( &! ) man in
	let cneg = Cp.cno in
	let inputs = Oops.array_make_n_var man mymodule.input in
	let eval clauses = Oops.list_et (&!) (List.map (fun clause -> Oops.list_or (&!) (List.map (fun (b, x) -> cneg b inputs.(x)) clause)) clauses) in
	{
		tacx_name = "CNF";
		tacx_man  = man;
		tacx_input= MyList.init mymodule.input string_of_int;
		tacx_edges= ["OUT", eval mymodule.clauses]
	}
;;

let my_tacx = upgrade my_cnf;;

Cp.TACX.dumpfile my_tacx.tacx_man (List.map (fun (name, edge) -> edge) my_tacx.tacx_edges) file_out;;

exit 0;;
