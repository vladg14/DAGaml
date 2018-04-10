(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

let string_of_vec = StrUtil.catmap""(function true -> "1" | false -> "0")
let print_vec vec = print_string (string_of_vec vec)

let entropy_sev =
	let rec aux i carry = function
		| [] -> carry
		| head::tail -> if head
			then (aux (i+1) carry tail)
			else (aux i (carry+i) tail)
	in aux 0 0

let dummydump_sev sev =
	"["^(StrUtil.catmap "; " (function None -> "None" | Some v -> "Some ("^(StrUtil.catmap "" (function true -> "1" | false -> "0") v)^")") sev )^"]"

let binload_sev : bool list -> bool list -> bool list option list =
	let rec aux i carry matx = function
		| [] -> carry
		| head::tail -> if head
			then aux (i+1) (None::carry) matx tail
			else
			(
				let v, matx = MyList.hdtl_nth i matx in
				aux i ((Some v)::carry) matx tail
			)
	in aux 0 [] (* matx *) (* main *)

let main = [true; false]
and matx = [true]

let entr = entropy_sev main
let () =
	print_string "entropy = "; print_int entr; print_newline();
	assert(List.length matx = entr);;


let strdump_sev =
	let iuniq_elem n =
		let rec aux carry = function
			| ([], []) -> (MyList.ntimes " " n)@("+"::(List.rev carry))
			| (None::x', y::y') -> aux ((if y then "+" else " ")::carry) (x', y')
			| ((Some _)::x', y') -> aux (" "::carry) (x', y')
			| _ -> assert false
		in fun vec tail -> aux [] (tail, vec)
	in
	let rec aux n floor matrix = function
		| [] -> String.concat "\n" ((List.map (String.concat "") ((List.rev matrix)@[List.rev floor])))
		| head::tail -> match head with
			| None		-> aux (n+1) ("_"::floor) matrix tail
			| Some v	-> aux (n+1) ("I"::floor) ((iuniq_elem n v tail)::matrix) tail
	in
	aux 0 [] []
;;

let sev = binload_sev matx main;;

print_newline();
print_string (dummydump_sev sev);
print_newline();
print_string (strdump_sev sev);
print_newline();;

open IterExtra

let gen_vec' size next	=
	Iter.gen_bool $^ size $+ next

let gen_vec  size		= gen_vec' size Iter.stop

let gen_sev' size	=
	(gen_vec size) $@++ (fun vect ->
		gen_vec (entropy_sev vect) $$+ (fun matx ->
			binload_sev matx vect
			))
;;

let gen_sev  size	= gen_sev' size Iter.stop
;;

let gen_rinni' =
	let get_last : bool list -> bool * ((bool list) option) =
		let rec aux some carry = function
			| [] -> assert false
			| [x] -> (x, if some then Some (List.rev carry) else None)
			| x::x' -> aux (some||x) (x::carry) x'
		in aux false []
	in
	fun size ->	(gen_vec size) $@++	(fun vect ->
		let vect = true::vect in
		(gen_vec (entropy_sev vect)) $$+	(fun matx ->
			let sev = binload_sev matx vect in
			let sev, last = MyList.last sev in
			assert(last = None);
			List.map (function None -> None | Some v -> Some (get_last v)) sev
											)
									)

let gen_rinni size = gen_rinni' size Iter.stop

let gen_ripair' =
	let merge =
		let rec aux carry = function
			| ([], []) -> List.rev carry
			| ([], _ ) -> assert false
			| ((None, None)::x', y::y') -> aux ((NniGops.ISS y)::carry) (x', y')
			| (x::x', y') -> let head = NniGops.(match x with
				| (None, None)		-> assert false
				| (None, Some v)	-> ISP v
				| (Some v, None)	-> IPS v
				| (Some v, Some v') -> IPP (v, v')) in aux (head::carry) (x', y')
		in fun uXY v -> aux [] (uXY, v)
	in 
	fun size ->
	let gu = gen_rinni size in
	gu $* gu $@++ (fun (uX, uY) ->
		let uXY = List.combine uX uY in
		let m = MyList.count (function (None, None) -> true | _ -> false) uXY in
		let funmap = merge uXY in
		gen_vec m $$+ funmap)

let gen_ripair size = gen_ripair' size Iter.stop

let gen_riuniq' size : NniTypes.edge_state Iter.next =
	(gen_vec size) $@++ (fun vect ->
		let vect = true::vect in
		let m = MyList.count_true vect in
		gen_vec (entropy_sev vect) $@++ (fun matx ->
			let funmap = NniGops.uniq_of_rsev_rvec (binload_sev matx vect) in
			(gen_vec m) $$+ funmap
			))
;;

let gen_riuniq  size	= gen_riuniq' size Iter.stop
;;

let gen_riuniq_comp' size =
	(gen_riuniq size) $@++ (fun ((_, uniq) as nni) ->
		let size' : int = NniGops.count_s uniq in
		(gen_riuniq size') $$+ (fun nni' -> (nni, nni')))
;;

let gen_riuniq_comp size = gen_riuniq_comp' size Iter.stop
;;

let gen_riuniq_comp_comp' size =
	(gen_riuniq size) $@++ (fun ((_, uniq) as nni) ->
		let size' : int = NniGops.count_s uniq in
		(gen_riuniq size') $@++ (fun ((_, uniq') as nni') ->
			let size'' : int = NniGops.count_s uniq' in
			(gen_riuniq size'') $$+ (fun nni'' -> (nni, nni', nni''))))
;;

let gen_riuniq_comp_comp size = gen_riuniq_comp_comp' size Iter.stop
;;

let n = 4;;

let g  = n $< gen_sev;;
let g2 = n $< (fun n -> let g = gen_sev n in (g$*g));;
let g3 = n $< (fun n -> let g = gen_sev n in ((g$*g)$*g));;

print_string "TEST 0 : strdump_sev"; print_newline();;
Iter.iter (fun sev -> ignore(strdump_sev sev);) g;;

let print_sev sev =
	try
		print_string(strdump_sev sev);
	with _ -> print_string(dummydump_sev sev);
	print_newline();
;;

print_string "TEST 1 : sev = reduce(expand(sev))"; print_newline();;
Iter.iter (fun sev -> assert(sev = Bsev.sev_reduce (Bsev.sev_expand sev));) g;;


print_string "TEST 2 : A U B = B U A"; print_newline();;
Iter.iter	(fun (sevA, sevB) ->
	let sevA' = Bsev.sev_expand sevA
	and sevB' = Bsev.sev_expand sevB in
	let sevAB = Bsev.sev_union sevA' sevB' |> Bsev.sev_reduce
	and sevBA = Bsev.sev_union sevB' sevA' |> Bsev.sev_reduce in
	if not(sevAB = sevBA)
	then
	(
		print_newline();
		print_string "sevA:";
		print_newline();
		print_sev sevA;
		print_newline();
		print_string "sevB:";
		print_newline();
		print_sev sevB;
		print_newline();
		print_string "sevAB:";
		print_newline();
		print_sev sevAB;
		print_newline();
		print_string "sevBA:";
		print_newline();
		print_sev sevBA;
		print_newline();
		assert(false);
	)
			) g2;;

print_string "TEST 2.1 : A = A U A"; print_newline();;
Iter.iter	(fun sevA ->
	let sevA' = Bsev.sev_expand sevA in
	let sevAA = Bsev.sev_union sevA' sevA' |> Bsev.sev_reduce in
	if not(sevA = sevAA)
	then
	(
		print_newline();
		print_string "sevA:";
		print_newline();
		print_sev sevA;
		print_newline();
		print_string "sevAA:";
		print_newline();
		print_sev sevAA;
		print_newline();
		assert(false);
	)
			) g;;


print_string "TEST 2.2 : (A U B) U C = A U (B U C)"; print_newline();;
Iter.iter	(fun ((sevA, sevB), sevC) ->
	let sevA' = Bsev.sev_expand sevA
	and sevB' = Bsev.sev_expand sevB
	and sevC' = Bsev.sev_expand sevC in
	let (+) = Bsev.sev_union in
	let sev1 = (sevA' + sevB') + sevC' |> Bsev.sev_reduce
	and sev2 = sevA' + (sevB' + sevC') |> Bsev.sev_reduce in
	if not(sev1 = sev2)
	then
	(
		print_newline();
		print_string "sevA:";
		print_newline();
		print_sev sevA;
		print_newline();
		print_string "sevB:";
		print_newline();
		print_sev sevB;
		print_newline();
		print_string "sev1 = (A+B)+C :";
		print_newline();
		print_sev sev1;
		print_newline();
		print_string "sev2 = A+(B+C) :";
		print_newline();
		print_sev sev2;
		print_newline();
		assert(false);
	)
			) g3;;

print_string "TEST 2.3 : ((x in A) or (x in B)) => (x in (A U B))"; print_newline();;
Iter.iter	(fun (sevA, sevB) ->
	let n = List.length sevA in
	assert(n = List.length sevB);
	let sevA' = Bsev.sev_expand sevA
	and sevB' = Bsev.sev_expand sevB in
	let (+) = Bsev.sev_union in
	let sevAB' = sevA' + sevB' in
	assert(n = List.length sevAB');
	let is_in = Bsev.sev_vec_in in
	Iter.iter	(fun v ->
		assert(n = List.length v);
		let inAB = is_in sevAB' v
		and inA  = is_in sevA'  v
		and inB  = is_in sevB'  v in
		assert(inA <= inAB);
		assert(inB <= inAB);
				) (gen_vec n);
			) g2;;

let gnni = n $< gen_riuniq;;
let gnni2 = n $< (fun n -> let g = gen_riuniq n in (g$*g));;

print_string "TEST 3 : NniGops.strdump_edge [terminate]"; print_newline();;
Iter.iter (fun nni -> ignore(NniGops.strdump_edge nni)) gnni;;


let print_nni nni =
	try
		print_string(NniGops.strdump_edge nni);
	with _ -> print_string "PRINTING FAILURE";
	print_newline();;


print_string "TEST 3.1 : NniGops.uniq_check"; print_newline();;
Iter.iter (fun nni -> assert(NniGops.edge_check nni);) gnni;;

print_string "TEST 3.2 : nni = binload(bindump(nni))"; print_newline();;
Iter.iter NniGops.(fun nni ->
	let nni' = bindump_edge nni in
	let nni'' = binload_edge nni' in
	if nni <> nni''
	then
	(
		print_string "nni:"; print_newline();
		print_nni nni;
		print_newline();
		print_string "nni' = bindump(nni):"; print_newline();
		print_string (Bitv.L.to_string nni');
		print_newline();
		print_string "nni'' = binload(nni'):"; print_newline();
		print_nni nni'';
		print_newline();
		assert false;
	);) gnni;;

print_string "TEST 4 : NniGops.solve_*_* [terminate]"; print_newline();;
Iter.iter NniGops.(fun (nniA, nniB) ->
	ignore(solve_cons_1 nniA nniB);
	ignore(solve_and_1	nniA nniB);
	ignore(solve_xor_1	nniA nniB);
	ignore(solve_cons_0 nniA nniB);)
		gnni2;;

print_string "TEST 5 : NniGops.compose [terminate]"; print_newline();;
Iter.iter NniGops.(fun (nniA, nniB) ->
	ignore(NniGops.compose nniA (nniB, ()));)
		(n $< gen_riuniq_comp);;

print_string "TEST 5.1 : A o (B o C) = (A o B) o C"; print_newline();;
Iter.iter NniGops.(fun (nniA, nniB, nniC) ->
	let ( * ) x y = let z, () = NniGops.compose x (y, ()) in z in
	let ab = nniA * nniB
	and bc = nniB * nniC in
	let abc1 = ab * nniC
	and abc2 = nniA * bc in
	if not(abc1 = abc2)
	then
	(
		print_newline();
		print_string "nniA:";
		print_newline();
		print_nni nniA;
		print_newline();
		print_string "nniB:";
		print_newline();
		print_nni nniB;
		print_newline();
		print_string "nniC:";
		print_newline();
		print_nni nniC;
		print_newline();
		print_string "ab = A o B :";
		print_newline();
		print_nni ab;
		print_newline();
		print_string "abc1 = (A o B) o C :";
		print_newline();
		print_nni abc1;
		print_newline();
		print_string "bc = B o C :";
		print_newline();
		print_nni abc2;
		print_newline();
		print_string "abc2 = A o (B o C) :";
		print_newline();
		print_nni abc2;
		print_newline();
		assert(false);
	);)
		(n $< gen_riuniq_comp_comp);;


print_string "TEST 6 : get_rinni [terminate]"; print_newline();;
Iter.iter ignore (n $< gen_rinni);;

let gp = n $< gen_ripair;;

print_string "TEST 7 : get_ripair [terminate]"; print_newline();;
Iter.iter ignore gp;;

print_string "TEST 7.1 : ipair = ipair_of_pair(pair_of_ipair ipair)"; print_newline();;
Iter.iter NniGops.(fun pair -> assert(pair = ipair_of_pair(pair_of_ipair pair));) (n $< gen_ripair);;

print_string "TEST 7.2 : ipair = binload(bindump ipair)"; print_newline();;
Iter.iter NniGops.(fun pair -> assert(pair = ipair_of_pair(binload_pair(bindump_pair(pair_of_ipair pair))));) gp;;

print_string "TEST 7.3 : consistency between pair_split, uniqX_of_pair and uniqY_of_pair"; print_newline();;
Iter.iter NniGops.(fun pair ->
	let pair = pair_of_ipair pair in
	let uX, uY = pair_split pair in
	let uX' = uniqX_of_pair pair
	and uY' = uniqY_of_pair pair in
	assert((uX = uX') && (uY = uY'));
					) gp;;

print_string "TEST 7.4 : consistency between pair_split, uniq_is_root, pair_is_root"; print_newline();;
Iter.iter NniGops.(fun pair ->
	let pair = pair_of_ipair pair in
	let uX, uY = pair_split pair in
	let rX = uniq_is_root uX
	and rY = uniq_is_root uY in
	let rX', rY' = pair_is_root pair in
	assert((rX = rX') && (rY = rY'));
					) gp;;
