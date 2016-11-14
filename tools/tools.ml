type ('a, 'b) ab = A of 'a | B of 'b

let cmp a b =
	if (a = b)
		then 0
	else if (a < b)
		then 1
		else (-1)


let cswap c (a, b) = if c then (a, b) else (b, a)

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


let opmax x = function
	| Some y	-> Some (max x y)
	| None		-> Some x

let opmin x = function
	| Some y	-> Some (min x y)
	| None		-> Some x

let opopmin x y = match x, y with
    | None, None -> None
    | Some x, Some y -> Some (min x y)
    | ((Some _) as r), None | None, ((Some _) as r) -> r

let opmm x = function
	| Some (mini, maxi)	-> Some (min mini x, max maxi x)
	| None				-> Some (x, x)



let rec math_pow x = function
	| 0	-> 1
	| 1	-> x
	| n when n mod 2 = 0	-> math_pow (x*x) (n/2)
	| n						-> x*(math_pow (x*x) (n/2))

let math_log x =
	let rec aux = function
		| 0 -> assert false
		| 1 -> 0
		| n when n < x	-> 1
		| n				-> 1+(aux (n/x))
	in aux

let string_of_option string_of = function
	| None -> "None"
	| Some x -> "Some ("^(string_of x)^")"



let check func objet =
	assert(func objet);
	objet
