(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

type t = {
	arity : int;
	array : Cp.TACX.edge array; (* uint = sum(b_i 2^i) *)
}

let get_arity t = t.arity
let get_array t = t.array

let of_int arity x =
	let bin = Tools.bin_of_int x in
	let array = Array.map (fun b -> Oops.make_const b arity) bin in
	{
		arity = arity;
		array = array
	}

let get myuint i =
	if i < Array.length myuint.array
	then myuint.array.(i)
	else (Oops.make_const false myuint.arity)

let length x = Array.length x.array

let fulladd man = 
	let ( && ) = Cp.( &! ) man
	and ( ^ ) = Cp.( ^! ) man in
	let ( || ) = Oops.(|!) ( && ) in fun a b c ->
	let carry =  (a || (b && c)) && (b || c) in
	let somme = a ^ b ^ c in
	(somme, carry)

let access2 x y =
	assert(x.arity = y.arity);
	let ari = x.arity in
	let len = max (length x) (length y) in
	let x = get x
	and y = get y in
	(ari, len, x, y)


let add man =
	let fulladd = fulladd man in fun x y ->
	let ari, len, x, y = access2 x y in
	let rec aux c i =
		if i < len
		then
		(
			let s, c = fulladd c (x i) (y i) in
			s::(aux c (i+1))
		)
		else
			[c]
	in
	{
		arity = ari;
		array = Array.of_list(aux (Oops.make_const false ari) 0)
	}

let addi man =
	let add = add man in
	(fun x y -> add x (of_int x.arity y))

let eq man =
	let ( == ) = Oops.(=!) (Cp.(^!) man)
	and ( & ) = Cp.(&!) man in fun x y ->
	let ari, len, x, y = access2 x y in
	if (len = 0)
	then Oops.make_const true ari
	else (Oops.vect_et (&) (Array.init len (fun i -> (x i) == (y i))))

let lX default man : t -> t -> Cp.TACX.edge =
	let ( =! ) = Oops.(=!) (Cp.(^!) man)
	and ( && ) = Cp.(&!) man in
	let ( || ) = Oops.(|!) ( && ) in
	let ite c x y = (c && x) || ((Cp.no c)&& y) in
	let ( <! ) x y = (Cp.no x) && y in fun x y ->
	let ari, len, x, y = access2 x y in
	let rec aux c i =
		if i < len
		then
		(
			let x = x i
			and y = y i in
			let c = ite ( x =! y ) c ( x <! y ) in
			aux c (i+1)
		)
		else c
	in aux (Oops.make_const default ari) 0

let lt = lX false
let le = lX true

let gX default man =
	let aux = lX (not default) man in
	fun x y -> Cp.no(aux x y)

let gt = gX false
let ge = gX true


let copy ncopy myuint =
	let mat = Oops.copy_funvect ncopy myuint.arity myuint.array in
	Array.map (fun array -> { arity = ncopy * myuint.arity; array = array }) mat

let copy_t ncopy myuint =
	let mat = Oops.copy_funvect_t ncopy myuint.arity myuint.array in
	Array.map (fun array -> { arity = ncopy * myuint.arity; array = array }) mat


let input man =
	let nvar = Oops.array_make_n_var man in fun n ->
	{
		arity = n;
		array = nvar n
	}

let range man =
	let (<=!) = le man
	and input = input man in fun n ->
	let arity = Array.length (Tools.bin_of_int (n-1)) in
	let myuint = input arity in
	myuint, (myuint <=! (of_int arity (n-1)))

let input_r man =
	let nvar = Oops.array_make_n_var man in fun n ->
	{
		arity = n;
		array = nvar n |> Array.to_list |> List.rev |> Array.of_list
	}

let range_r man =
	let (<=!) = le man
	and input = input_r man in fun n ->
	let arity = Array.length (Tools.bin_of_int (n-1)) in
	let myuint = input arity in
	myuint, (myuint <=! (of_int arity (n-1)))

let cat x fxs y fys =
	let xS = MyList.ntimes (CpTypes.S) x.arity
	and xP = MyList.ntimes (CpTypes.P) x.arity
	and yS = MyList.ntimes (CpTypes.S) y.arity
	and yP = MyList.ntimes (CpTypes.P) y.arity in
	let sp = xS @ yP
	and ps = xP @ yS in
	{
		arity = x.arity + y.arity;
		array = Array.map (Oops.(->>) sp) x.array
	},
	Array.map (Oops.(->>) sp) fxs,
	{
		arity = x.arity + y.arity;
		array = Array.map (Oops.(->>) ps) y.array
	},
	Array.map (Oops.(->>) ps) fys


