(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)
module type MODELE =
sig
	type edge
	val ( ->> ) : bool list -> edge -> edge
	val arity : edge -> int
	(* where true represent significant variables and false non significant ones *)
	val    neg : edge -> edge
	val ( &! ) : edge -> edge -> edge
	val ( |! ) : edge -> edge -> edge
	val ( ^! ) : edge -> edge -> edge
	val ( =! ) : edge -> edge -> edge
	val ( *! ) : edge -> edge -> edge

	val cst : bool -> int -> edge

	(* return a consistent projection of edges
		 to [Some bool] or [None] otherwise *)
	val to_bool : edge -> bool option
end

module MODULE(M0:MODELE) =
struct
	include M0

	let var	b n = (* match b with false -> id | true -> neg *)
		assert(n>=1);
		(cst b (n-1)) *! (cst (not b) (n-1))
	let cst0 = cst false 0
	let cst1 = cst true  0

	let ite cond edge0 edge1 = (cond &! edge1)|!((neg cond)&! edge0)

	let array_make_n_var n =
		let var0 = var false 1 in
		Array.init n (fun x -> (Tools.subset true false x n) ->> var0)

	let  list_make_n_var n =
		let var0 = var false 1 in
		MyList.init n (fun x -> (Tools.subset true false x n) ->> var0)

	let array_and array = Tools.tree_of_array (&!) array
	let  list_and  list =
		list |> Array.of_list |> array_and

	let init_and size f = array_and (Array.init size f)

	let array_or  array = Tools.tree_of_array (|!) array
	let  list_or   list =
		list |> Array.of_list |> array_and

	let  init_or size f = array_or  (Array.init size f)

	let array_neg array = Array.map neg array
	let  list_neg  list =  List.map neg list

	let array_at_least_one = array_or
	let  list_at_least_one =  list_or

	let array_exactly_one array =
		let n = Array.length array in
		assert(n>=1);
		let size = arity array.(0) in
		let neg_array = array_neg array in
		(* [tempX.(i)] iff forall k < i, not array.(k)
		   [tempY.(i)] iff forall k > i, not array.(k) *)
		let tempX = Array.make n (cst true size)
		and tempY = Array.make n (cst true size) in
		for i = 0 to (n-2)
		do
			tempX.(i+1)	<- tempX.(i) &! neg_array.(i);
		done;
		for i = (n-1) downto 1
		do
			tempY.(i-1) <- tempY.(i) &! neg_array.(i);
		done;
		init_or n (fun i -> tempX.(i) &! tempY.(i) &! array.(i))

	let array_atmost_one array =
		assert(Array.length array >= 1);
		let one = array_exactly_one array
		and noone = array_and (array_neg array) in
		one |! noone

	let array_copy_fun nb (func : edge) =
		let size = arity func in
	(* f -> [f..., .f.., ..f., ...f] *)
		let tn = MyList.ntimes true  size
		and fn = MyList.ntimes false size in
		let funmap idx = Tools.subset_t tn fn idx nb ->> func in
		Array.init nb funmap

	let array_copy_fun_t nb (func : edge) =
		let size = arity func in
	(* f -> [f(0)...f(1)..., .f(0)...f(1).., etc.] *)
		let funmap idx =
			MyList.ncopy (Tools.subset true false idx nb) size
			->> func
		in
		Array.init nb funmap

	let array_copy_fun_array ncopy (array : edge array) =
		let nfunc = Array.length array in
		array
			|> Array.map (array_copy_fun ncopy)
			|> Matrix.matrix_of_array2 nfunc ncopy
			|> Matrix.transpose

	let array_copy_fun_array_t ncopy (array : edge array) =
		let nfunc = Array.length array in
		array
			|> Array.map (array_copy_fun_t ncopy)
			|> Matrix.matrix_of_array2 nfunc ncopy
			|> Matrix.transpose
end



