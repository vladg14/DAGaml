(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

let make_const  b n = Cp.TACX.push_leaf (b, MyList.ntimes CpTypes.P n) ()

let make_ident man b n = Cp.( *! ) man (make_const b n) (make_const (not b) n)

let push_pass = Cp.push_pass

let ( =?? ) = Cp.( =?? )

let no = Cp.no

let t0 = make_const true 0
let f0 = make_const false 0
let t1 man = make_ident man false 0 (* function x -> x  *)
let f1 man = make_ident man true 0  (* function x -> -x *)

let (|!) (&!) x y = no ((no x) &! (no y))
let (=!) (^!) x y = no (x ^! y)

let (-->) x y : Cp.TACX.edge = CpGops.compose x y
let (->>) uniq y : Cp.TACX.edge = CpGops.compose ((false, uniq):CpTypes.edge_state) y


let array_make_n_var man n =
	let var = t1 man
	and subset = Tools.subset (CpTypes.S) (CpTypes.P) in
	Array.init n (fun x -> (subset x n) ->> var)

let list_make_n_var man n = Array.to_list(array_make_n_var man n)

let vect_et (&!) vect =
	assert ((Array.length vect)>0);
	let rec aux i j =
		assert (i < j);
		if (j-i) = 1
			then vect.(i)
		else(
			let c = (i+j)/2 in
			(aux i c) &! (aux c j)
			)
	in aux 0 (Array.length vect)

let list_et (&!) liste = vect_et (&!) (Array.of_list liste)

let vect_no l = Array.map no l

let list_no l = List.map  no l

let vect_or (&!) vect = no (vect_et (&!) (vect_no vect))

let list_or (&!) liste = vect_or (&!) (Array.of_list liste)

let vect_atleast_one = vect_or;;

let vect_exactly_one (&!) size vect =
	let n = Array.length vect in

	let not_vect = Array.map no vect in

	let accu_and_not = Array.make n (make_const true size)
	and ucca_and_not = Array.make n (make_const true size) in

	for i = 0 to (n-2)
	do
		accu_and_not.(i+1)	<- accu_and_not.(i) &! not_vect.(i);
	done;

	for i = (n-1) downto 1
	do
		ucca_and_not.(i-1)	<- ucca_and_not.(i) &! not_vect.(i);
	done;
	
	let one_vect = Array.init n
		(fun x -> accu_and_not.(x) &! ucca_and_not.(x) &! vect.(x)) in
	
	vect_or (&!) one_vect

let vect_atmost_one (&!) size vect =
	let (|!) = (|!) (&!) in
	let one = vect_exactly_one (&!) size vect
	and noone = vect_et (&!) (vect_no vect) in
	one |! noone

(*
let store_ALLsat pshi psi target =
	let allSat_memo, allSat = Recs.defALLsat pshi 100000 in
	let file = open_out target in
	let output_string = output_string file  in
	let allsat = allSat psi in
	print_int (List.length allsat);
	print_newline();
	List.iter (fun sol -> output_string (OutP10.to_string sol); output_string "\n") allsat;
	close_out file

let print_ALLsat pshi psi =
	let allSat_memo, allSat = Recs.defALLsat pshi 100000 in
	let allsat = allSat psi in
	print_int (List.length allsat);
	print_newline();
	List.iter (fun sol -> print_string (OutP10.to_string sol); print_newline()) allsat
*)

let copy_fun copy (func : Cp.TACX.edge) =
	let size = Cp.arity func in
(* f -> [f..., .f.., ..f., ...f] *)
	let tn = MyList.ntimes (CpTypes.S) size
	and fn = MyList.ntimes (CpTypes.P) size in
	let subset idx = Tools.subset_t tn fn idx copy in
	Array.init copy (fun idx -> subset idx ->> func)

let copy_fun_t copy func =
	let size = Cp.arity func in
(* f -> [f(0)...f(1)..., .f(0)...f(1).., etc.] *)
	let subset idx = MyList.ncopy (Tools.subset CpTypes.S CpTypes.P idx copy) size in
	Array.init copy (fun idx -> subset idx ->> func)

let transpose n m mat =
	Array.init m (fun x -> Array.init n (fun y -> mat.(y).(x)))

let copy_funvect ncopy arity (funvect : Cp.TACX.edge array) =
	let nfunc = Array.length funvect in
	let mat = Array.map (copy_fun ncopy) funvect in
	transpose nfunc ncopy mat

let copy_funvect_t ncopy arity (funvect : Cp.TACX.edge array) =
	let nfunc = Array.length funvect in
	let mat = Array.map (copy_fun_t ncopy) funvect in
	transpose nfunc ncopy mat


