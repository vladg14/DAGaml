open Extra

type 'a return =
	| Stop
	| Elem of 'a * ('a iter)
and 'a iter = unit -> 'a return
type 'a next = 'a iter -> 'a iter

let gen_bool =
	function() -> Elem(true,
	function() -> Elem(false,
	function() -> Stop))	

let range x y =
	let rec aux x () =
		if x < y
		then Elem (x, aux (x + 1))
		else Stop
	in
	aux x

let map f i =
	let rec aux i () = match i() with
		| Stop			-> Stop
		| Elem(x, i')	-> Elem(f x, aux i')
	in
	aux i

let ( $$ ) i f = map f i

let map' f i next =
	let rec aux i () = match i() with
		| Stop			-> next()
		| Elem(x, i')	-> Elem(f x, aux i')
	in aux i

let ( $$+ ) i f next = map' f i next

let fold_left0 sx x0 i =
	let rec aux s i = match i() with
		| Stop			-> s
		| Elem(x, i')	-> aux (sx s x) i'
	in aux x0 i

let find sx i =
	let rec aux i = match i() with
		| Stop         -> None
		| Elem(x, i') -> match sx x with
			| Some y   -> Some y
			| None     -> aux i
	in aux i

let exists sx i =
	let rec aux i = match i() with
		| Stop -> false
		| Elem(x, i') -> (sx x)||(aux i)
	in aux i

let for_all sx i =
	let rec aux i = match i() with
		| Stop -> true
		| Elem(x, i') -> (sx x)&&(aux i)
	in aux i

let ( $! ) i sx x0 = fold_left0 sx x0 i

let fold_left sx i =
	match i() with
		| Stop			-> None
		| Elem(x0, i')	-> Some(fold_left0 sx x0 i')

let ( $!! ) i sx = fold_left sx i

let of_list l =
	let rec aux = function
		| []	-> (function () -> Stop)
		| x::l'	-> (function () -> Elem(x, aux l'))
	in aux l

let iter f i =
	let rec aux i = match i() with
		| Stop			-> ()
		| Elem(x0, i')	-> f x0; aux i'
	in aux i

let count i =
	let rec aux n i = match i() with
		| Stop			-> n
		| Elem(x, i')	-> aux (if x then (n+1) else n) i'
	in aux 0 i

let length i =
	let rec aux n i = match i() with
		| Stop			-> n
		| Elem(x, i')	-> aux (n+1) i'
	in aux 0 i

let stop = function () -> Stop
let is_stop i = match i() with Stop	-> true | _ -> false
let pull i = match i() with
	| Stop			-> None
	| Elem(x, i')	-> Some (x, i')
let push x i = function () -> Elem(x, i)

let fill_None (a_op_iter as opi)  (a_iter as i) =
	let rec aux opi i () = match opi() with
		| Stop -> assert(is_stop i); Stop
		| Elem(item, opi') ->	match item with
			| None ->	( match i() with
				| Stop -> assert false
				| Elem(item', i') -> Elem(item', aux opi' i')
						)
			| Some item	-> Elem(item, aux opi' i)
	in aux opi i

let fill_None_partial opI opi =
	let rec aux opI opi () = match opI() with
		| Stop -> assert(is_stop opi); Stop
		| Elem(item, opI') -> match item with
			| None ->	( match opi() with
				| Stop -> assert false
				| Elem(item', opi') -> Elem(item', aux opI' opi')
						)
			| Some item -> Elem(Some item, aux opI' opi)
	in aux opI opi

let fill_None_default default opi =
	let rec aux opi () = match opi() with
		| Stop -> Stop
		| Elem(item, opi')	->	(
			let item' = match item with None -> default | Some x -> x in
			Elem(item', aux opi')
								)
	in aux opi

let zip i1 i2 =
	let rec aux i1 i2 () = match i1(), i2() with
		| Stop, Stop -> Stop
		| Elem(x1, i1'), Elem(x2, i2') -> Elem((x1, x2), aux i1' i2')
		| Stop, Elem _ | Elem _ , Stop -> assert false
	in aux i1 i2

let enumerate n i =
	let rec aux n i () = match i() with
		| Stop -> Stop
		| Elem(x, i') -> Elem((n, x), aux (n+1) i')
	in
	aux n i

let progress head step init iter =
	let rec aux n i () = match i() with
		| Stop -> (print_string head; print_int n; print_newline(); Stop)
		| Elem(x, i') -> ((if n mod step = 0 then (print_string head; print_int n; print_string "\r"; flush stdout)); Elem(x, aux (n+1) i'))
	in aux init iter

let iter_while f x0 =
	let rec aux x () = match f x with
		| None		-> Stop
		| Some x'	-> Elem(x', aux x')
	in aux x0

let iter_fold f x0 =
	let rec aux x () = match f x with
		| None			-> Stop
		| Some(x', y')	-> Elem(y', aux x')
	in aux x0


let unop' opi next =
	let rec aux opi () =
		let rec auxaux opi = match opi() with
			| Stop -> next()
			| Elem(opx, opi') -> match opx with
				| None		-> auxaux opi'
				| Some x	-> Elem(x, aux opi')
		in auxaux opi
	in aux opi

let unop opi = unop' opi stop

let ( $? ) i f = unop (i $$ f)

let filter' f i next = unop' (i $$ (fun x -> if f x then Some x else None)) next

let filter f i = filter' f i stop

let bool_a_pair_to_option' i next = map' ((function (true, x) -> Some x | (false, _) -> None):((bool*'a)->'a option)) i next

let bool_a_pair_to_option i = bool_a_pair_to_option' i stop

let pair_filter' i next = unop' (bool_a_pair_to_option i) next

let pair_filter i = pair_filter' i stop

let inv_filter' iF i next =
	let rec aux iF i () = match iF() with
		| Stop -> assert(is_stop i); next()
		| Elem(x, iF') -> match x with
			| true ->	(match i() with
				| Stop -> assert false;
				| Elem(y, i') -> Elem(Some y, aux iF' i')
						)
			| false ->	Elem(None, aux iF' i)
	in aux iF i

let inv_filter iF i = inv_filter' iF i stop

let to_list i =
	let rec aux carry i = match i() with
		| Stop			-> List.rev carry
		| Elem(x, i')	-> aux (x::carry) i'
	in aux [] i

let to_list_partial n i =
	let rec aux carry = function
		| 0 -> (fun i -> List.rev carry, i)
		| n -> assert(n > 0); fun i -> match i() with
			| Stop -> assert false
			| Elem(x, i') -> aux (x::carry) (n-1) i'
	in assert(n>=0); aux [] n i

let ( $+ ) i1 i2 =
	let rec aux i1 () = match i1() with
		| Stop			-> i2()
		| Elem(x, i1')	-> Elem(x, aux i1')
	in aux i1

let add_fst'	x i = map'	(fun y -> (x, y)) i
let add_fst  x i = add_fst' x i stop
let add_snd'	i y = map'	(fun x -> (x, y)) i
let add_snd  x i = add_snd' x i stop

let compose'' gen' i next =
	let rec aux i () = match i() with
		| Stop			-> next()
		| Elem(x, i')	-> gen' x (aux i') ()
	in aux i

let ( $@++ ) i gen' next = compose'' gen' i next
let ( $<++ ) n gen' next = compose'' gen' (range 0 n) next

let compose' gen = compose'' (fun x next -> gen x $+ next)
let ( $@+  ) i gen next = compose' gen i next
let ( $<+  ) n gen next = compose' gen (range 0 n) next

let compose gen i = compose' gen i stop
let ( $@   ) i gen = compose gen i
let ( $<   ) n gen = compose gen (range 0 n)

let map_compose' f i j next = compose'' (fun x next -> map' (f x) j next) i next
let ( $$@+ ) i f j next = map_compose' f i j next

let map_compose f i j = map_compose' f i j stop
let ( $$@  ) i f j = map_compose f i j

let ( $*+ ) i j next	= map_compose' (fun x y -> (x, y)) i j next
let ( $*  ) i j			= map_compose  (fun x y -> (x, y)) i j

let add_tail' i t = map' (fun x -> x::t) i
let add_tail  i t = add_tail' i t stop
let add_head' x i = map' (fun t -> x::t) i;;
let add_head  x i = add_head' x i stop

let ( $:+ ) i j next	= map_compose' (fun x t -> x::t) i j next
let ( $:  ) i j			= map_compose  (fun x t -> x::t) i j

let ( $:$+ ) i j f next = map_compose' (fun x t -> f(x::t)) i j next
let ( $:$  ) i j f		= map_compose  (fun x t -> f(x::t)) i j

let inlist' i = map' (fun x -> [x]) i
let inlist  i = map  (fun x -> [x]) i

let ( $^+ ) i n next =
	let rec aux = function
		| 1 -> inlist i
		| n -> i $: (aux (n - 1))
	in match n with
		| 0 -> push [] next
		| 1 -> inlist' i next
		| n -> assert(n>1); i $:+ aux (n - 1) <| next

let ( $^ ) i n = ( $^+ ) i n stop

let ( $^$+ ) i n f next =
	let rec aux = function
		| 1 -> inlist i
		| n -> i $: (aux (n - 1))
	in	(match n with
			| 0 -> push []
			| 1 -> i $$+ (fun x -> f[x])
			| n -> assert(n>1); ($:$+) i (aux (n - 1)) f
		) next

let ( $^$ ) i n f = ($^$+) i n f stop

let eval i = i |> to_list |> of_list;;

let flatten' ii next =
	let rec aux ii () = match ii() with
		| Stop			-> next()
		| Elem(i, ii')	-> (i $+ (aux ii'))()
	in aux ii

let flatten ii = flatten' ii stop

let half_square i =
	let rec aux k i : _ list iter = match k with
		| 1 ->	inlist i
		| k ->	match i() with
			| Stop			-> stop
			| Elem(x, i')	->
				add_head' x (aux (k-1) i) (aux k i')
	in function
		| 0 -> push [] stop
		| 1 -> inlist i
		| k -> assert(k > 1); aux k i


let half_square_strict i =
	let rec aux = function
		| 1 ->	inlist
		| k ->	(fun i () -> match i() with
			| Stop			-> Stop
			| Elem(x, i')	-> (add_head' x (aux (k-1) i') (aux k i'))()
				)
	in function
		| 0 -> push [] stop
		| 1 -> inlist i
		| k -> assert(k > 1); aux k i
