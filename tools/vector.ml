open Extra

type t = bool list

let cons x = x
let uncons x = x

let ophdtl = function
	| [] -> None
	| head::tail -> Some (head, tail)

let ophd = function
	| [] -> None
	| head::_ -> Some head

let optl = function
	| [] -> None
	| _::tail -> Some tail

let hd = List.hd
let tl = List.tl
let hdtl x = (List.hd x, List.tl x)

let hdtl_nth = MyList.hdtl_nth

let nones n = MyList.ntimes true n
let nzero n = MyList.ntimes false n

let first l = MyList.list_index true l

let one_zeros n = true::(nzero n)
let zeros_one = one_zeros >> List.rev

let (@) = (@)

let count = MyList.count_true
let size = List.length

let string_of = StrUtil.catmap "" StrUtil.string_of_bool
let pretty_of = StrUtil.catmap "" StrUtil.pretty_of_bool

let print_t vect = Format.print_string (string_of vect)

let bools = [false; true]

let nonull vect = List.exists (fun x -> x) vect
let isnull vect = not (nonull vect)

let xor (a:bool) b = a!=b
let add = List.map2 xor
let cadd = function
	| true -> add
	| false -> (fun x _ -> x)
let no l = l ||> (not)

let map_vector size func = Maps.map_power bools size (cons >> func)

let cond_vector cond_vect vect =
	List.combine cond_vect vect ||> (function (true, x) -> Some (x:bool) | _ -> None) |> MyList.list_of_oplist

let cond_filter cond_vect liste =
	List.combine cond_vect liste ||> (function (true, x) -> Some x | _ -> None) |> MyList.list_of_oplist

