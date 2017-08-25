type ('a, 'b) t = {
	table : ('a, 'b) Hashtbl.t;
	mutable hitCnt : int;
	mutable clcCnt : int;
}

let create n = {
	table = Hashtbl.create n;
	hitCnt = 0;
	clcCnt = 0;
}

let test mem a = Hashtbl.mem (mem.table) a;;
let push mem a b =
	if test mem a
	then failwith "MemoTable - already stored"
	else Hashtbl.add (mem.table) a b

let memo mem a b = push mem a b; b;;
let pull mem a = Hashtbl.find (mem.table) a;;

let apply mem fonc a =
	if test mem a
	then(
		mem.hitCnt <- mem.hitCnt + 1;
		pull mem a
		)
	else(
		mem.clcCnt <- mem.clcCnt + 1;
		memo mem a (fonc a)
		)

let print_stats mem = 
	print_string 	"MemoTable's length:\t";
	print_int (Hashtbl.length (mem.table));
	print_string	"\nMemoTable's HitCnt:\t";
	print_int mem.hitCnt;
	print_string	".\nMemoTable's ClcCnt:\t";
	print_int mem.clcCnt;
	print_string	".\n"

let dump_stats mem = Tree.Node [
		Tree.Node [Tree.Leaf "length:"; StrTree.of_int (Hashtbl.length (mem.table))];
		Tree.Node [Tree.Leaf "hit count:"; StrTree.of_int mem.hitCnt];
		Tree.Node [Tree.Leaf "clc count:"; StrTree.of_int mem.clcCnt]
	]

let make n = let mem = create n in ( mem, apply mem )

