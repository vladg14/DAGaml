(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open IterExtra
open Cpx2BTypes
open Cpx2BUtils

let gen_assign n = (Iter.of_list [None; Some false; Some true]) $^ n

let check_contig m : bool =
	let n = List.length m in
	let t = Array.make n 0 in
	List.iter (fun x -> t.(x) <- t.(x)+1) m;
	let rec aux0 = function
		| [] -> true
		| 0::tail -> aux0 tail
		| _::tail -> false
	in
	let rec aux1 : int list -> bool = function
		| [] -> true
		| 0::tail -> aux0 tail
		| _::tail -> aux1 tail
	in
	aux1 ((Array.to_list t):int list)



let dummydump m = "["^(StrUtil.catmap ", " string_of_int m)^" ]"

let gen1 n = (Iter.range 0 n) $^ n |> (Iter.filter check_contig)
let gen_contig = gen1 ;;

let gen_vec n = Iter.gen_bool $^ n;;

let gen_sub' n = (Iter.range 0 3) $^ n $@++ (fun vect ->
	let n2 = MyList.count(function 2 -> true | _ -> false) vect in
	(gen_contig n2) $* (gen_vec n2) $$+ (fun (a, b) ->
		let rec f carry = function
			| ([], []) -> List.rev carry
			| (0::x, y) -> f (S::carry) (x, y)
			| (1::x, y) -> f (P::carry) (x, y)
			| (2::x, (i, b)::y) -> f(X(b, i)::carry) (x, y)
			| _ -> assert false
		in f [] (vect, List.combine a b)))


let gen_sub n = gen_sub' n Iter.stop;;

let gen_block n = (Iter.gen_bool $* Iter.gen_bool $* gen_sub n) $$
	(fun ((neg, shift), liste) ->
		let tag = get_spx_tag_from_spx liste in
		{neg; arity = n; block = SPX(shift, tag, liste)}
	)

let gen_block_checked_next_is_leaf n = gen_block n |> (Iter.filter (fun block -> check_block block true))

let gen_block_checked n = gen_block n |> (Iter.filter (fun block -> check_block block false))

let _ = List.iter print_string (MyList.init 10 string_of_int); print_newline()
