open Extra

let list_init n f =
	assert(n>=0);
	let rec aux carry = function
		| 0 -> carry
		| n -> aux ((f(n-1))::carry) (n-1)
	in aux [] n

let init = list_init

let catlist (cat: 'a) : 'a list list -> 'a list = function
	| [] -> []
	| [x] -> x
	| x::x' -> x@(List.flatten (List.map (fun (x:'a list) -> cat::x) x'))

let option_cons = function
	| None -> (fun l -> l)
	| Some x -> (fun l -> (x::l))

let catmap f l = List.flatten (List.map f l)

let list_of_oplistv0 opel = List.fold_right option_cons opel []

let list_of_oplistv1 oplist = List.map (function Some x -> x | None -> assert false) (List.filter (function Some _ -> true | None -> false) oplist)

let list_of_oplistv3 =
	let rec aux = function
		| [] -> []
		| None::oplist' -> aux oplist'
		| (Some item)::oplist' -> item::(aux oplist')
	in aux

let list_of_oplistv4 =
	let rec aux carry = function
		| [] -> List.rev carry
		| None::oplist' -> aux carry oplist'
		| (Some item)::oplist' -> aux (item::carry) oplist'
	in (fun l -> aux [] l)

let list_of_oplist = list_of_oplistv4

let unop = list_of_oplist

let opmap opfun liste = list_of_oplist (List.map opfun liste)
let opmap2 opfun liste1 liste2 = list_of_oplist (List.map2 opfun liste1 liste2)

let sum = List.fold_left (+) 0

let count f l = sum(l||>(fun x -> if f x then 1 else 0))
let counti f l = sum(List.mapi (fun i x -> if (f i x) then 1 else 0)l)

let count_true = count (fun x -> x)
let counti_true = counti (fun _ x -> x)

let ntimes x =
	let rec aux carry = function
		| 0 -> carry
		| n -> aux (x::carry) (n-1)
	in aux []

let make n x = ntimes x n

let ncopy x =
	let rec aux carry = function
		| 0 -> carry
		| n -> aux (x@carry) (n-1)
	in aux []


let listfilter filt flist =
	let rec aux filt flist plist = match filt, flist with
		| [], []	-> List.rev plist
		| [], _		-> failwith "listfilter failure - non coherent - 22"
		| _, []		-> failwith "listfilter failure - non coherent - 23"
		| true::filt, x::flist	-> aux filt flist (x::plist)
		| false::filt, x::flist	-> aux filt flist plist
	in aux filt flist []

let consensus0v1 merge =
	let rec aux p (f0, p0) (f1, p1) = match f0, f1 with
		| [], []	-> (List.rev p, List.rev p0, List.rev p1)
		| [], _		-> failwith "consensus failure - non coherent - 81"
		| _, []		-> failwith "consensus failure - non coherent - 82"
		| x0::f0, x1::f1 -> let x, x0, x1 = merge x0 x1 in
			aux (x::p) (f0, option_cons x0 p0) (f1, option_cons x1 p1)
	in (fun f0 f1 -> aux [] (f0, []) (f1, []))

let consensus0v2 merge f0 f1 =
	let p, p01 = List.map2 (fun x y -> let x, y, z = merge x y in x, (y, z)) f0 f1 |> List.split in
	let p0, p1 = List.split p01 in
	(p, list_of_oplist p0, list_of_oplist p1)

let consensus0 = consensus0v1

let consensusi0 merge =
	let rec aux i p (f0, p0) (f1, p1) = match f0, f1 with
		| [], []	-> (List.rev p, List.rev p0, List.rev p1)
		| [], _		-> failwith "consensus failure - non coherent - 90"
		| _, []		-> failwith "consensus failure - non coherent - 91"
		| x0::f0, x1::f1 -> let x, x0, x1 = merge i x0 x1 in
			aux (i+1) (x::p) (f0, option_cons x0 p0) (f1, option_cons x1 p1)
	in (fun f0 f1 -> aux 0 [] (f0, []) (f1, []))

let consensus merge =
	let rec aux (e, p) (e0, f0, p0) (e1, f1, p1) = match f0, f1 with
		| [], []	-> ((e, List.rev p), (e0, List.rev p0), (e1, List.rev p1))
		| [], _		-> failwith "consensus failure - non coherent - 99"
		| _, []		-> failwith "consensus failure - non coherent - 100"
		| x0::f0, x1::f1 -> let (e, x), (e0, x0), (e1, x1) = merge e (e0, x0) (e1, x1) in
			aux (e, x::p) (e0, f0, option_cons x0 p0) (e1, f1, option_cons x1 p1)
	in (fun e (e0, f0) (e1, f1) -> aux (e, []) (e0, f0, []) (e1, f1, []))


let onehash_check hash hashed = List.for_all (fun item -> hashed = (hash item))
let onehash hash = function
	| [] -> true
	| head::tail -> onehash_check hash (hash head) tail

let get_onehash hash = function
	| [] -> None
	| head::tail ->
		let hashed = hash head in
		assert(onehash_check hash hashed tail);
		Some hashed

let lists_length_check size lists = onehash_check List.length size lists
let lists_length lists = onehash List.length lists
let lists_get_length lists = get_onehash List.length lists

let list_index item =
	let rec aux pos = function
		| [] -> None
		| head::tail ->
			if item=head
			then Some pos
			else (aux (pos+1) tail)
	in aux 0

let index p =
	let rec aux pos = function
		| [] -> None
		| head::tail ->
			if p head
			then Some pos
			else (aux(pos+1) tail)
	in aux 0

let ifind (p : 'a -> 'b option) : 'a list -> (int * 'b) option =
	let rec aux pos = function
		| [] -> None
		| head::tail -> match p head with
			| None -> aux (pos+1) tail
			| Some obj -> Some(pos, obj)
	in aux 0

let hdtl = function
	| []	-> assert false
	| x::y	-> x, y

let map_hdtl vects =
	List.split (List.map hdtl vects)


let hdtl_nth n liste =
	let rec aux carry = function
		| 0, next -> (List.rev carry, next)
		| n, [] -> assert false
		| n, head::tail -> aux (head::carry) (n-1, tail)
	in
	assert (n>=0);
	assert ((List.length liste)>=n);
	let head, tail = aux [] (n, liste) in
	assert((List.length head)=n);
	head, tail


let list_add_partial_to_support default support partial =
	let auxend support = List.map (fun liste -> default::liste) support in
	let rec aux carry = function
		| support, [] -> (List.rev carry)@(auxend support)
		| [], _		-> assert false
		| head_support::tail_support, head_partial::tail_partial ->
			aux ((head_partial::head_support)::carry) (tail_support, tail_partial)
	in aux [] (support, partial)

let list_transpose_partial_matrix default size partial_matrix =
	let rec parcours support = function
		| [] -> List.map List.rev support
		| head::tail -> parcours (list_add_partial_to_support default support head) tail
	in
	assert(size>=0);
	parcours (list_init size (fun _ -> [])) partial_matrix


let member x l = List.exists ((=)x) l

let int_of_boollist =
	let rec aux acc = function
		| [] -> acc
		| head::tail -> aux (2*acc+(if head then 1 else 0)) tail
	in aux 0


let rec list_uniq = function
	| []	-> []
	| [a]	-> [a]
	| a::b::next ->	(
		if a=b
		then list_uniq (b::next)
		else a::(list_uniq (b::next))
					)

let string_of_list string_of liste = String.concat "" ["["; String.concat "; " (List.map string_of liste); "]"]

let print_list string_of liste = print_string (string_of_list string_of liste)

let list_map_z1 func = 
	let rec aux item = function
		| [] -> []
		| head::tail -> (func item head)::(aux head tail)
	in function
		| [] -> []
		| head::tail -> aux head tail


let indexify filter =
	let rec aux index = function
		| [] -> []
		| head::tail -> if (filter head)
			then ((Some index)::(aux (index+1) tail))
			else (None::(aux index tail))
	in aux 0

let indexify_true = indexify (fun x -> x)


let foldmap func check init =
	let rec aux carry fold = function
		| [] -> (List.rev carry, fold)
		| head::tail ->
		(
			(*(check fold) is true*)
			let head', fold' = func fold head in
			assert(check fold');
			aux (head'::carry) fold' tail
		)
	in
	assert(check init);
	aux [] init


let opfoldmap func check init =
	let rec aux carry fold = function
		| [] -> (List.rev carry, fold)
		| head::tail ->	(
			(*(check fold) is true*)
			let head', fold' = func head fold in
			assert(check fold');
			match head' with
				| Some head' -> aux (head'::carry) fold' tail
				| None -> aux carry fold' tail
						)
	in
	assert(check init);
	aux [] init


let list_iterative_reduction func =
	let rec aux carry obj =
		let opitem, opobj' = func obj in
		let carry' = option_cons opitem carry in
		match opobj' with
			| Some obj'	-> aux carry' obj'
			| None		-> List.rev carry'
	in aux []

let last =
	let rec aux carry = function
		| []			-> assert false
		| [x]			-> List.rev carry, x
		| head::tail	-> aux (head::carry) tail
	in function
		| [] -> assert false
		| liste -> aux [] liste

let setnth liste x e =
	assert(0 <= x && x < (List.length liste));
	List.mapi (fun i -> if i = x then (fun _ -> e) else (fun y -> y)) liste
