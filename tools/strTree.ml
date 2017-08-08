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
	| Tree.Node treelist -> String.concat " " ("("::(treelist ||> dump_tree)@[")"])

let smart_dump_tree print =
	let rec aux = function
		| Tree.Leaf text -> print(dump_leaf text)
		| Tree.Node treelist -> print "( "; List.iter (fun tree -> aux tree; print " ") treelist; print " )";
	in aux


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

let dumpfile treelist target =
	let file = open_out target in
	let output_string = output_string file in
	List.iter (fun tree -> smart_dump_tree output_string tree; output_string "\n") treelist;
	close_out file;
	()


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

let loadfile target =
	let stream = Stream.of_channel (open_in target) in
	StrTreeParser.str_tree_parser stream
(*	let file = open_in target in
	let read() =
		try Some(input_line file)
		with End_of_file ->
		(
			close_in file;
			None
		)
	in
	let rec loop carry = match read () with
		| Some line -> loop (line::carry)
		| None -> String.concat "\n" (List.rev carry)
	in
	load (loop [])
*)
		

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


let of_list of_a liste = Tree.Node (List.map of_a liste)
let to_list to_a = function
	| Tree.Node liste -> List.map to_a liste
	| Tree.Leaf _ -> assert false

let of_array of_a array = of_list of_a (Array.to_list array)
let to_array to_a stree = Array.of_list (to_list to_a stree)



(* get colored version of str *)
let colorize color str =
	if color > 0
	then "\027[" ^ (string_of_int color) ^ "m" ^ str ^ "\027[0m"
	else str


type enum =
	| T000
	| T001
	| T010
	| T011
	| T100
	| T101
	| T110
	| T111

(* print colored tree *)
let tree_print print =
	(* draw UTF-8 tree line *)
	let conv = function
		| T000 -> "  "
		| T001 -> "┌ "
		| T010 -> "──"
		| T011 -> "┌─"
		| T100 -> "└ "
		| T101 -> "│ "
		| T110 -> "└─"
		| T111 -> "├─"
	in
	let print_row row =
		print (StrUtil.catmap""conv(List.rev row));
	in
	let rec print_tree row0 rows = function
		| Tree.Leaf leaf	-> print_row row0; print " "; print leaf; print "\n";
		| Tree.Node liste	-> match liste with
			| []		-> print_row row0; print "|\n";
			| [x]		-> print_tree (T010::row0) (T000::rows) x
			| x::next	-> print_tree (T011::row0) (T101::rows) x; print_tree_list rows next;
	and print_tree_list row = function
		| tree::[]		-> print_tree (T110::row) (T000::row) tree
		| tree::next	-> print_tree (T111::row) (T101::row) tree; print_tree_list row next
		| []			-> ()
	in List.iter (print_tree [] [])
