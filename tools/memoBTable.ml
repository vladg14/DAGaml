type ('a, 'aa, 'b, 'bb) t = {
	o3sA : ('a, 'aa) O3.o3;
	o3sB : ('b, 'bb) O3.o3;
	mutable err : (int -> 'a option -> 'aa option -> 'b option -> 'bb option -> unit);
	mutable each : ('a -> 'aa -> 'b -> 'bb -> unit);
	table : ('aa, 'bb) Hashtbl.t;
	hitCnt : int ref;
	clcCnt : int ref;
}

let create o3sA o3sB n = {
	o3sA; o3sB;
	err = (fun x _ _ _ _ ->
		print_newline();
		print_string "MemoBTable - apply error: "; print_int x;
		print_newline()
	);
	each = (fun _ _ _ _ -> ());
	table = Hashtbl.create n;
	hitCnt = ref 0;
	clcCnt = ref 0;
}

let test mem a = Hashtbl.mem mem.table (fst mem.o3sA a);;
let push mem a b =
	if test mem a
	then failwith "MemoTable - already stored"
	else Hashtbl.add mem.table a (fst mem.o3sB b)

let memo mem a b = push mem a b; b;;
let pull mem a = Hashtbl.find mem.table (fst mem.o3sA a) |> snd mem.o3sB;;

let nocheck_apply mem fonc a =
	let aa = fst mem.o3sA a in
	try
		let bb = Hashtbl.find mem.table aa in
		incr mem.hitCnt;
		snd mem.o3sB bb
	with Not_found ->
	(
		incr mem.clcCnt;
		let b = fonc a in
		let bb = fst mem.o3sB b in
		Hashtbl.add mem.table aa bb;
		b
	)

let apply mem fonc a =
	let err x = mem.err x (Some a) in
	let aa = try fst mem.o3sA a with exn -> (err 0 None None None; raise exn) in
	let err x = err x (Some aa) in
	let a' = try snd mem.o3sA aa with exn -> (err 1 None None; raise exn) in
	if not(a = a') then (err 2 None None; assert false);
	try
		let bb = Hashtbl.find mem.table aa in
		let err x opb = err x opb (Some bb) in
		incr mem.hitCnt;
		let b = try snd mem.o3sB bb with exn -> (err 3 None; raise exn) in
		let err x = err x (Some b) in
		let bb' = try fst mem.o3sB b with exn -> (err 4; raise exn) in
		if not(bb = bb') then (err 5; assert false);
		b
	with Not_found ->
	(
		incr mem.clcCnt;
		let b = try fonc a with exn -> (err 6 None None; raise exn) in
		let err x = err x (Some b) in
		let bb = try fst mem.o3sB b with exn -> (err 7 None; raise exn) in
		mem.each a aa b bb;
		let err x = err x (Some bb) in
		let b' = try snd mem.o3sB bb with exn -> (err 8; raise exn) in 
		if not (b = b') then (err 9; assert false);
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

let dump_stats mem = Tree.Node [
		Tree.Node [Tree.Leaf "length:"; StrTree.of_int (Hashtbl.length (mem.table))];
		Tree.Node [Tree.Leaf "hit count:"; StrTree.of_int (!(mem.hitCnt))];
		Tree.Node [Tree.Leaf "clc count:"; StrTree.of_int (!(mem.clcCnt))]
	]

let make o3sA o3sB n =
	let mem = create o3sA o3sB n in
	( mem, (if false then apply else nocheck_apply) mem )

let nocheck_make o3sA o3sB n =
	let mem = create o3sA o3sB n in
	( mem, nocheck_apply mem )
