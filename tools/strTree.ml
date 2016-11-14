open Extra
type tree = string Tree.tree

let dump_leaf text =
	match StrUtil.split ' ' (String.escaped text) with
		| []	-> "\"\""
		| [x]	-> "\""^x^"\""
		| lx	-> "[ \""^(String.concat "\" \"" lx)^"\" ]"

let unpack_text text =
	let x = String.get text 0 in
	let y = String.sub text 1 (String.length text - 2) in
	let z = String.get text (String.length text - 1) in
	assert(x = '"');
	assert(z = '"');
	Scanf.unescaped y

let load_leaf strlist =
	let rec aux carry = function
		| []	-> assert false
		| head::tail -> match head with
			| "]"	-> Tree.Leaf (String.concat " " (List.rev carry)), tail
			| text	-> aux ((unpack_text text)::carry) tail
	in
	aux [] strlist

let rec dump_tree = function
	| Tree.Leaf text -> dump_leaf text
	| Tree.Node treelist -> String.concat " " ("("::(List.map dump_tree treelist)@[")"])

let rec load_tree strlist =
	let rec aux carry = function
		| []	-> assert false
		| head::tail -> match head with
			| ")"	-> Tree.Node (List.rev carry), tail
			| "("	-> let tree, tail' = load_tree tail in aux (tree::carry) tail'
			| "["	-> let leaf, tail' = load_leaf tail in aux (leaf::carry) tail'
			| text	-> aux ((Tree.Leaf (unpack_text text))::carry) tail
	in aux [] strlist

let dump treelist = StrUtil.catmap "\n" dump_tree treelist

let print treelist =
	print_string(dump treelist);
	print_newline()

let load text =
	let strlist = List.filter (function "" -> false | _ -> true) (StrUtil.split ' ' (StrUtil.explode text ||> (function ' ' | '\n' | '\t' -> ' ' | x -> x) |> StrUtil.implode)) in
	let rec aux carry = function
		| [] -> List.rev carry
		| head::tail -> match head with
			| "("	-> let tree, tail' = load_tree tail in aux (tree::carry) tail'
			| "["	-> let leaf, tail' = load_leaf tail in aux (leaf::carry) tail'
			| text	-> aux ((Tree.Leaf (unpack_text text))::carry) tail
	in aux [] strlist

let to_pretty =
	let lvlstr lvl text = (StrUtil.ntimes " " lvl)^text^"\n" in
	let rec aux lvl = function
		| Tree.Leaf text -> lvlstr lvl text
		| Tree.Node treelist -> (lvlstr lvl "(")^(StrUtil.catmap "" (aux(lvl+1)) treelist)^(lvlstr lvl ")")
	in StrUtil.catmap "\n" (aux 0)

let pprint treelist =
	print_string(to_pretty treelist);
	print_newline()

let of_string x = Tree.Leaf x
let to_string = function
	| Tree.Leaf x	-> x
	| _				-> assert false

let of_bool = StrUtil.string_of_bool >> of_string
let to_bool = to_string >> StrUtil.bool_of_string

let of_int = string_of_int >> of_string
let to_int = to_string >> int_of_string

let of_unit () = Tree.Node []
let to_unit = function
	| Tree.Node [] -> ()
	| _ -> assert false

let of_option of_a = function
	| None		-> Tree.Node []
	| Some a	-> Tree.Node [of_a a]

let to_option to_a = function
	| Tree.Node x	->	(match x with
		| []	-> None
		| [a]	-> Some(to_a a)
		| _		-> assert false
						)
	| _				->	assert false

