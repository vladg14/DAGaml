open Expr
open IoUtils

type token =
	| Kwd of string
	| Sym of char
	| Ident of string

let my_lexer stream : token Stream.t =
	let is_space = function ' ' | '\t' | '\n' -> true | _ -> false in
	let rec spaces () = match Stream.peek stream with
	| None -> ()
	| Some head -> if is_space head then (Stream.junk stream; spaces ()) else ()
	in
	let is_symbol = function '=' | '~' | '&' | '|' | '^' | '(' | ')' | ';' | ',' -> true | _ -> false in
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
		| "assign"
		| "module"
		| "endmodule"
		| "input"
		| "output"
		| "wire"
		| "1'b0"		-> Kwd w
		| _				-> Ident w
	in
	let rec aux x : token option=
		spaces ();
		match Stream.peek stream with
		| None -> None
		| Some head ->
		Some (
			assert(not(is_space head));
			if is_symbol head
			then (Stream.junk stream; Sym head)
			else (word ())
		)
	in
	Stream.from aux

(*let pstring x = print_string x; print_string " "; flush stdout*)
let pstring _ = ()
(*let pchar x = print_char x; print_char ' '; flush stdout*)
let pchar _ = ()

let rec parse_leaf stream = match Stream.next stream with
	| Ident var -> pstring var; PVar var
	| Sym '(' ->
	(
		pstring "(";
		let expr = parse_expr stream in
		PUop (PNop, expr)
	)
	| Sym '~' ->
	(
		pstring "~";
		let expr = parse_leaf stream in
		PUop (PNot, expr)
	)
	| Kwd "1'b0" ->
	(
		pstring "1'b0";
		PCst false
	)
	| _ -> assert false

and     parse_expr stream =
	let first = parse_leaf stream in
	match Stream.next stream with
	| Sym ';' -> pstring "\n"; first
	| Sym ')' -> pstring ")"; first
	| Sym '&' ->
	(
		pstring "&";
		let second = parse_expr stream in
		PBop (PAnd, first, second)
	)
	| Sym '|' ->
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
(*parse_coma_list stream*)
	)
	| _ -> assert false


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
	let rec aux input output wire assign =
	match Stream.next stream with
	| Kwd "endmodule" -> List.{
		expr_name   = mymodule.expr_name;
		expr_param  = mymodule.expr_param;
		expr_input  = input  |> rev |> MyList.flatten;
		expr_output = output |> rev |> MyList.flatten;
		expr_wire   = wire   |> rev |> MyList.flatten;
		expr_assign = assign |> rev
	}
	| Kwd "input" ->
	(
		let liste = parse_semicolon_list stream in
		aux (liste::input) output wire assign
	)
	| Kwd "output" ->
	(
		let liste = parse_semicolon_list stream in
		aux input (liste::output) wire assign
	)
	| Kwd "wire" ->
	(
		let liste = parse_semicolon_list stream in
		aux input output (liste::wire) assign
	)
	| Kwd "assign" ->
	(
		let item = parse_assign stream in
		aux input output wire (item::assign)
	)
	| _ -> assert false
	in aux
		[mymodule.expr_input]
		[mymodule.expr_output]
		[mymodule.expr_wire]
		(List.rev mymodule.expr_assign)


let parse_module stream =
	let rec aux () = match Stream.next stream with
	| Kwd "module" ->
	(
		let expr_name = parse_ident stream in
		let expr_param = parse_braket_list stream in
		parse_semicolon stream;
		let module_expr = {
			expr_name;
			expr_param;
			expr_input=[];
			expr_output=[];
			expr_wire=[];
			expr_assign=[]
		} in
		let mymodule = parse_module_aux module_expr stream in
		print_newline();
		mymodule;
	)
	| _ -> aux ()
	in aux ()


let load stream = stream |> my_lexer |> parse_module

let load_file file =
	let fin = open_in file in
	let mexpr = load (Stream.of_channel fin) in
	close_in fin;
	mexpr
