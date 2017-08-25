type ('a, 'b) ab = A of 'a | B of 'b

let isSome  = function Some _ -> true  | None -> false
let isNone  = function Some _ -> false | None -> true
let isOk    = function Ok _ -> true  | Error _ -> false
let isError = function Ok _ -> false | Error _ -> true

let mapreduce map reduce init liste = List.fold_left (fun x y -> reduce x (map y)) init liste

let cmp a b =
	if (a = b)
		then 0
	else if (a < b)
		then 1
		else (-1)


let cswap c (a, b) = if c then (b, a) else (a, b)
let op2 op (a, b) = (op a, op b)
let opop (opa, opb) (a, b) = (opa a, opb b)

let cond_fun = function
	| true	-> (fun f x -> f x)
	| false	-> (fun f x ->   x)

let op_fun f = function
	| None		-> None
	| Some x	-> Some(f x)

let string_of_bool = function
	| true	-> "1"
	| false	-> "0"

let print_bool x = print_string (string_of_bool x)


let subset special default =
	let rec aux = function
		| _, 0		-> []
		| 0, size	-> special::(aux((-1), (size-1)))
		| idx, size	-> default::(aux((idx-1), (size-1)))
	in (fun idx size -> assert((idx>=0)&&(size>=0)&&(idx<size)); aux(idx, size))

let subset_t special default =
	let rec aux = function
		| _, 0		-> []
		| 0, size	-> special@(aux((-1), (size-1)))
		| idx, size	-> default@(aux((idx-1), (size-1)))
	in (fun idx size -> assert((idx>=0)&&(size>=0)&&(idx<size)); aux(idx, size))


let opfold_left f x = function
	| Some y -> Some(f x y)
	| None   -> Some x

let opfold_right f = function
	| Some x -> (fun y -> Some(f x y))
	| None   -> (fun y -> Some y)

let opopfold f x y = match x, y with
	| None  , None   -> None
  | Some z, None
	| None  , Some z -> Some z
  | Some x, Some y -> Some (f x y)
	

let opmax x = opfold_left max x
let opmin x = opfold_left min x

let opopmin x = opopfold min x

let opmm x = function
	| Some (mini, maxi)	-> Some (min mini x, max maxi x)
	| None				-> Some (x, x)



let rec math_pow = function
	| 0 -> (function 0 -> 1 | _ -> 0)
	| 1 -> (fun _ -> 1)
	| 2 -> (fun x -> 1 lsl x)
	| x -> (function
		| 0	-> 1
		| 1	-> x
		| n when n mod 2 = 0	-> math_pow (x*x) (n/2)
		| n						-> x*(math_pow (x*x) (n/2))
	)

let quick_pow (unit : 'a) (( * ) : 'a -> 'a -> 'a) (x : 'a) (k : int) : 'a =
	let rec aux = function
		| 0 -> (fun _ -> unit)
		| 1 -> (fun x -> x   )
		| n when n mod 2 = 0 -> (fun x ->       aux (n/2) (x*x)   )
		| n                  -> (fun x -> x * ( aux (n/2) (x*x) ) )
	in
	aux k x

let math_log x =
	let rec aux = function
	(*	| n when n <= 0	-> assert false *)
		| 1 -> 0
		| n when n < x	-> 1
		| n				-> 1+(aux (n/x))
	in (fun x -> assert(x > 0); aux x)

let math_log_up x y =
	assert(x > 0);
	assert(y > 0);
	let z = math_log x y in
	let z' = if y > (math_pow x z)
		then (z+1)
		else z
	in
	assert(z' >= 0);
	if z = 0
		then (assert(y = 1))
		else (assert(math_pow x (z'-1) < y && y <= math_pow x z'));
	z'

let string_of_option string_of = function
	| None -> "None"
	| Some x -> "Some ("^(string_of x)^")"



let check func objet =
	assert(func objet);
	objet

let bin_of_int =
	let rec aux x =
		if x = 0
		then []
		else (x mod 2 = 1) :: ( aux (x/2) )
	in (fun x -> assert(x>=0); Array.of_list (aux x))

let unop = function
	| None -> assert false
	| Some x -> x


let bin_of_char c =
	let rec aux carry x = function
		| 0 -> List.rev carry
		| n -> aux ((x mod 2 = 1)::carry) (x/2) (n-1)
	in aux [] (Char.code c) 8

let int_of_bin bl =
	let rec aux carry = function
		| [] -> carry
		| h::t -> aux (carry*2+(if h then 1 else 0)) t
	in aux 0 (List.rev bl)

let char_of_bin bl = Char.chr ( int_of_bin bl )

(* with [p] a predicate and [a] an array *)
let array_index (p : 'a -> bool) (a : 'a array) : int option =
	let n = Array.length a in
	let rec aux x =
			if x < n
			then (if p a.(x) then Some x else aux (x+1))
			else None
	in aux 0
