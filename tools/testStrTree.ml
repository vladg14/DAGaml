let l1 = [Tree.Leaf "a"];;

let d1 = StrTree.dump l1;;

print_string d1; print_newline();;
print_string (StrTree.to_pretty l1); print_newline();;

let l2 = StrTree.load d1;;

assert(l1 = l2);;

let l1 = [Tree.Leaf "a b"];;

let d1 = StrTree.dump l1;;

print_string d1; print_newline();;
print_string (StrTree.to_pretty l1); print_newline();;

let l2 = StrTree.load d1;;

assert(l1 = l2);;

let l1 = [Tree.Node [Tree.Leaf "a"; Tree.Leaf "a b"]];;

let d1 = StrTree.dump l1;;

print_string d1; print_newline();;
print_string (StrTree.to_pretty l1); print_newline();;

let l2 = StrTree.load d1;;

assert(l1 = l2);;

print_newline();;

let l = [
	"\"a\"";
	"[  \"a\"  \"b\"  ]";
	"(  \"a\"  [  \"a\"   \"b\"  ] )";
		];;

List.iter	(fun x ->
	print_string x; print_newline();
	x |> StrTree.load |> StrTree.dump |> print_string;
	print_newline();
	print_newline();
			) l;;



exit 0;;
