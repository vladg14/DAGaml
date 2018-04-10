open IoUtils
open Expr

type token =
	| Kwd of string
	| Sym of char
	| Ident of string

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
	let is_symbol = function '=' | '~' | '&' | '|' | '^' | '(' | ')' | ';' | ',' | '*' | '+' | '!' -> true | _ -> false in
	let word () =
		let rec aux s = match Stream.peek stream with
			| None -> StrUtil.implode (List.rev s)
			| Some c ->
			( if is_space c || is_symbol c
				then (StrUtil.implode (List.rev s))
				else (Stream.junk stream; aux (c::s))
			)
		in
		let w = aux [] in match w with
		| "INORDER"
		| "OUTORDER" -> Kwd w
		| _ -> Ident w
	in
	let rec aux x : token option=
		spaces ();
		match Stream.peek stream with
		| None -> None
		| Some head ->
		(
			if head = '#'
			then (rmline (); aux x)
			else
			(
				assert(not(is_space head));
				Some ( if is_symbol head
				then (Stream.junk stream; Sym head)
				else (word ()))
			)
		)
	in
	Stream.from aux


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

let pstring x = print_string x; print_string " "; flush stdout
let pstring x = ()
let pchar x = print_char x; print_char ' '; flush stdout
let pchar x = ()

let rec parse_leaf stream = match Stream.next stream with
	| Ident var -> pstring var; PVar var
	| Sym '(' ->
	(
		pstring "(";
		let expr = parse_expr stream in
		PUop (PNop, expr)
	)
	| Sym '~' | Sym '!' ->
	(
		pstring "~";
		let expr = parse_leaf stream in
		PUop (PNot, expr)
	)
	| _ -> assert false

and     parse_expr stream =
	let first = parse_leaf stream in
	match Stream.next stream with
	| Sym ';' -> pstring "\n"; first
	| Sym ')' -> pstring ")"; first
	| Sym '&' | Sym '*' ->
	(
		pstring "&";
		let second = parse_expr stream in
		PBop (PAnd, first, second)
	)
	| Sym '|' | Sym '+'->
	(
		pstring "|";
		let second = parse_expr stream in
		PBop (POr, first, second)
	)
	| Sym '^' ->
	(
		pstring "^";
		let second = parse_expr stream in
		PBop (PXor, first, second)
	)
	| x -> assert false


let parse_ident stream = match Stream.next stream with
	| Ident name -> name
	| _ -> assert false


let rec parse_coma_list stream =
	let rec aux carry = match Stream.peek stream with
	| None -> assert false
	| Some head -> match head with
		| Sym ',' ->
		(
			Stream.junk stream;
			let ident = parse_ident stream in
			aux (ident::carry)
		)
		| _ -> List.rev carry
	in
	match Stream.next stream with
	| Ident ident -> aux [ident]
	| _ -> assert false


let parse_braket_list stream = match Stream.next stream with
	| Sym '(' ->
	(
		let liste = parse_coma_list stream in
		match Stream.next stream with
		| Sym ')' -> liste
		| _ -> assert false
	)
	| _ -> assert false


let parse_list stream =
	let rec aux carry = match Stream.next stream with
	| Sym ';' -> List.rev carry
	| Ident ident -> aux (ident::carry)
	| _ -> assert false
	in aux []

let parse_semicolon stream = match Stream.next stream with
	| Sym ';' -> ()
	| _ -> assert false
	

let parse_semicolon_list stream =
	let liste = parse_coma_list stream in
	parse_semicolon stream;
	liste



let parse_kwd stream = match Stream.next stream with
	| Kwd kwd -> kwd
	| _ -> assert false


let parse_sym stream = match Stream.next stream with
	| Sym sym -> sym
	| _ -> assert false


let parse_assign stream =
	let ident = parse_ident stream in
	assert('=' = parse_sym stream);
	let expr = parse_expr stream in
	(ident, expr)
	

let parse_module_aux mymodule stream =
	let rec aux mymodule = try
		match Stream.next stream with
		| Kwd "INORDER" ->
		(
			assert('=' = parse_sym stream);
			let liste = parse_list stream in
			aux {
				expr_name    = mymodule.expr_name;
				expr_param  = mymodule.expr_param;
				expr_input   = mymodule.expr_input @ liste; 
				expr_output  = mymodule.expr_output;
				expr_wire    = mymodule.expr_wire;
				expr_assign  = mymodule.expr_assign;
			}
		)
		| Kwd "OUTORDER" ->
		(
			assert('=' = parse_sym stream);
			let liste = parse_list stream in
			aux {
				expr_name    = mymodule.expr_name;
				expr_param  = mymodule.expr_param;
				expr_input   = mymodule.expr_input; 
				expr_output  = mymodule.expr_output @ liste;
				expr_wire    = mymodule.expr_wire;
				expr_assign  = mymodule.expr_assign;
			}
		)
		| Ident ident ->
		(
			assert('=' = parse_sym stream);
			let expr = parse_expr stream in
			let assign = (ident, expr) in
			aux {
				expr_name    = mymodule.expr_name;
				expr_param  = mymodule.expr_param;
				expr_input   = mymodule.expr_input; 
				expr_output  = mymodule.expr_output;
				expr_wire    = mymodule.expr_wire;
				expr_assign  = mymodule.expr_assign @ [assign];
			}
		)
		| _ -> assert false
	with _ -> mymodule
	in aux mymodule

let module_expr = {
	expr_name = "";
	expr_param = [];
	expr_input = [];
	expr_output = [];
	expr_wire = [];
	expr_assign = []
}

let parse_module stream = parse_module_aux module_expr stream

let load stream = stream |> my_lexer |> parse_module
