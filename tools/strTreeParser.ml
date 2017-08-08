let str_tree_parser stream =
	let pull () =
		try	      Some(Stream.next stream)
		with _ -> None
	in
	let blank = function ' ' | '\t' | '\n' -> true | _ -> false in
	let parse_word () =
		let rec aux carry = match pull () with
			| None -> assert false
			| Some head -> match head with
				| '\\' -> let c = pull () |> Tools.unop in aux ('\\'::c::carry)
				| '"'  -> Scanf.unescaped (StrUtil.implode (List.rev carry))
				|  c   -> aux (c::carry)
		in aux []
	in
	let parse_leaf () =
		let rec aux carry = match pull () with
			| None -> assert false
			| Some head -> match head with
				| '"' -> let word = parse_word () in aux (word::carry)
				| ']' -> String.concat " " (List.rev carry)
				|  c when blank c -> aux carry
				| _ -> assert false
		in aux []
	in
	let rec parse_node () =
		let rec aux carry = match pull () with
			| None -> List.rev carry
			| Some head ->
			( match head with
				| '(' -> let anode = parse_node () in aux ((Tree.Node anode)::carry)
				| '[' -> let aleaf = parse_leaf () in aux ((Tree.Leaf aleaf)::carry)
				| '"' -> let aword = parse_word () in aux ((Tree.Leaf aword)::carry)
				| ')' -> List.rev carry
				|  c when blank c -> aux carry
				| _ -> assert false
			)
		in aux []
	in
	parse_node ()
