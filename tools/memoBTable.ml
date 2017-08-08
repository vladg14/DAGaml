type ('a, 'aa, 'b, 'bb) t = {
	dumpA : 'a -> 'aa;
	loadA : 'aa -> 'a;
	dumpB : 'b -> 'bb;
	loadB : 'bb -> 'b;
	table : ('aa, 'bb) Hashtbl.t;
	hitCnt : int ref;
	clcCnt : int ref;
}

let create dumpA loadA dumpB loadB n = {
	dumpA; loadA; dumpB; loadB;
	table = Hashtbl.create n;
	hitCnt = ref 0;
	clcCnt = ref 0;
}

let test mem a = Hashtbl.mem mem.table (mem.dumpA a);;
let push mem a b =
	if test mem a
	then failwith "MemoTable - already stored"
	else Hashtbl.add mem.table a (mem.dumpB b)

let memo mem a b = push mem a b; b;;
let pull mem a = Hashtbl.find mem.table (mem.dumpA a) |> mem.loadB;;

let nocheck_apply mem fonc a =
	let aa = mem.dumpA a in
	try
		let bb = Hashtbl.find mem.table aa in
		incr mem.hitCnt;
		mem.loadB bb
	with Not_found ->
	(
		incr mem.clcCnt;
		let b = fonc a in
		let bb = mem.dumpB b in
		Hashtbl.add mem.table aa bb;
		b
	)

let apply mem fonc a =
	let aa = mem.dumpA a in
	assert(a = mem.loadA aa);
	try
		let bb = Hashtbl.find mem.table aa in
		incr mem.hitCnt;
		let b = mem.loadB bb in
		assert(bb = mem.dumpB b);
		b
	with Not_found ->
	(
		incr mem.clcCnt;
		let b = fonc a in
		let bb = mem.dumpB b in
		assert(b = mem.loadB bb);
		Hashtbl.add mem.table aa bb;
		b
	)

let print_stats mem = 
	print_string 	"MemoTable's length:\t";
	print_int (Hashtbl.length (mem.table));
	print_string	"\nMemoTable's HitCnt:\t";
	print_int (!(mem.hitCnt));
	print_string	".\nMemoTable's ClcCnt:\t";
	print_int (!(mem.clcCnt));
	print_string	".\n"

let dump_stat mem = Tree.Node [
		Tree.Node [Tree.Leaf "length:"; StrTree.of_int (Hashtbl.length (mem.table))];
		Tree.Node [Tree.Leaf "hit count:"; StrTree.of_int (!(mem.hitCnt))];
		Tree.Node [Tree.Leaf "clc count:"; StrTree.of_int (!(mem.clcCnt))]
	]

let make dumpA loadA dumpB loadB n =
	let mem = create dumpA loadA dumpB loadB n in
	( mem, apply mem )

let nocheck_make dumpA loadA dumpB loadB n =
	let mem = create dumpA loadA dumpB loadB n in
	( mem, nocheck_apply mem )
