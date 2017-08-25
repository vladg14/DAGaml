type 'a matrix = {
	n : int;
	m : int;
	a : 'a array array;
}

let pretty_bool_matrix matrix =
	let pretty_line line = StrUtil.catmap "" (function true -> "X" | _ -> "-") (Array.to_list line) in
	StrUtil.catmap "\n" pretty_line (Array.to_list matrix.a)

let make (n, m) x = {n; m; a = Array.make_matrix n m x}
let init (n, m) f = {n; m; a = Array.init n (fun i -> Array.init m (fun j -> f(i, j)))}

let set mat (i, j) x = mat.a.(i).(j) <- x
let get mat (i, j)   = mat.a.(i).(j)

let sumi ( + ) (zero : 'a) n (f : int -> 'a) : 'a = Array.fold_left ( + ) zero (Array.init n f)

let multmat ( + ) ( * ) (zero : 'a) (matA : 'a matrix) (matB : 'a matrix) : 'a matrix=
	assert(matA.m = matB.n);
	init (matA.n, matB.m) (fun (i, j) -> sumi ( + ) zero matA.m (fun k -> matA.a.(i).(k) * matB.a.(k).(j)))

let expmat (( + ) : 'a -> 'a -> 'a) (( * ) : 'a -> 'a -> 'a) (zero : 'a) (unit : 'a) (mat : 'a matrix) (k : int) : 'a matrix=
	assert(mat.n = mat.m);
	let n = mat.n in
	let mult : 'a matrix -> 'a matrix -> 'a matrix = multmat ( + ) ( * ) zero in
	let mat1 = init (n, n) (fun(i, j) -> if i = j then unit else zero) in
	Tools.quick_pow mat1 mult mat k
