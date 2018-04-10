(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open NniTypes
open Extra


type iinvar = bool * (bool list option)

let count_s = MyList.count (function S _ -> true | P _ -> false)
let count_p = MyList.count (function S _ -> false | P _ -> true)


type iuniq_elem =
	| IS of bool
	| IP of iinvar

let count_is = MyList.count (function IS _ -> true | IP _ -> false)
let count_ip = MyList.count (function IS _ -> false | IP _ -> true)

type iuniq = iuniq_elem list

let iinvar_of_invar (b, opi) = (b, (match opi with
	| None -> None
	| Some i -> Some(Bitv.L.to_bool_list i)))

let invar_of_iinvar (b, opi) = (b, (match opi with
	| None -> None
	| Some i -> Some(Bitv.L.of_bool_list i)))

let xiuniq_of_xuniq = List.map (function
	| S b -> IS b
	| P i -> IP (iinvar_of_invar i))

let xuniq_of_xiuniq = List.map (function
	| IS b -> S b
	| IP i -> P (invar_of_iinvar i))

type ipair_elem =
	| ISS of bool
	| IPS of iinvar
	| ISP of iinvar
	| IPP of iinvar * iinvar

type ipair = ipair_elem list

let ipair_of_pair = List.map (function
	| SS b -> ISS b
	| PS i -> IPS (iinvar_of_invar i)
	| SP i -> ISP (iinvar_of_invar i)
	| PP (ix, iy) -> IPP (iinvar_of_invar ix, iinvar_of_invar iy))

let pair_of_ipair = List.map (function
	| ISS b -> SS b
	| IPS i -> PS (invar_of_iinvar i)
	| ISP i -> SP (invar_of_iinvar i)
	| IPP (ix, iy) -> PP (invar_of_iinvar ix, invar_of_iinvar iy))


let uniq_check =
	let rec aux i = function
		| [] -> true
		| head::tail -> match head with
			| S _ -> aux (i+1) tail
			| P (b, opv) -> (match opv with
				| None -> true
				| Some v -> Bitv.length v = i)&&(aux i tail)
	in List.rev >> (aux 0)

let edge_check (b, u) = uniq_check u

let riuniq_check =
	let rec aux i = function
		| [] -> true
		| head::tail -> match head with
			| IS _ -> aux (i+1) tail
			| IP (b, opv) -> (match opv with
				| None -> true
				| Some v -> List.length v = i)&&(aux i tail)
	in List.rev >> (aux 0)

let iuniq_check =
	let rec aux  = function
		| [] -> true
		| head::tail -> match head with
			| IS _ -> aux tail
			| IP (_, opv) -> (match opv with
				| None -> true
				| Some v -> List.length v = List.length tail)&&(aux tail)
	in aux

let strdump_riuniq b =
	let iuniq_elem n =
		let rec aux carry = function
			| ([], []) -> (MyList.ntimes " " (n+1))@("+"::(List.rev carry))
			| ((IS _)::x', y::y') -> aux ((if y then "+" else " ")::carry) (x', y')
			| ((IP _)::x', y') -> aux (" "::carry) (x', y')
			| _ -> assert false
		in fun vec tail -> aux [] (tail, vec)
	in
	let rec aux n floor matrix = function
		| [] -> String.concat "\n" ((List.map (String.concat "") ((List.rev matrix)@[(if b then "-" else "+")::(List.rev floor)])))
		| head::tail -> match head with
			| IS b				-> aux (n+1) ((if b then "F" else "S")::floor) matrix tail
			| IP (b, None)		-> aux (n+1) ((if b then "A" else "U")::floor) matrix tail
			| IP (b, Some v)	-> aux (n+1) ((if b then "N" else "P")::floor) ((iuniq_elem n v tail)::matrix) tail
	in
	aux 0 [] []

let strdump_edge (b, u) = strdump_riuniq b (xiuniq_of_xuniq u)

let bindump_invar (b, opi) (stream:bool list) : bool list =
	BinDump.bool b (BinDump.option BinDump.bitv opi stream)

let binload_invar i stream =
	let b, stream = BinLoad.bool stream in
	let opi, stream = BinLoad.option (BinLoad.bitv i) stream in
	(b, opi), stream

let bindump_uniq_elem stream = function
	| S b	-> false::b::stream
	| P i	-> true::(bindump_invar i stream)

let binload_uniq_elem i = BinLoad.choice
(* S *)	(fun stream ->
	let b, stream = BinLoad.bool stream in
	(S b, (i+1), stream)
		)
(* P *) (fun stream ->
	let invar, stream = binload_invar i stream in
	(P invar, i , stream)
		)

let bindump_uniq = (let rec fold stream = function
	| [] -> stream
	| head::tail -> fold (true::(bindump_uniq_elem stream head)) tail in fold (false::[]))

let binload_uniq = (
	let rec fold i carry = BinLoad.choice
		(fun stream -> carry)
		(fun stream ->
			let e, i, stream = binload_uniq_elem i stream in
			fold i (e::carry) stream)
	in fold 0 [])


let bindump_pair_elem carry = function
	| SS b					-> false::b::carry
	| SP invar				-> true::false::true::(bindump_invar invar carry)
	| PS invar				-> true::false::false::(bindump_invar invar carry)
	| PP (invarX, invarY)	-> true::true ::false::(bindump_invar invarX (bindump_invar invarY carry))

let bindump_pair = (let rec fold carry = function
	| [] -> carry
	| head::tail -> fold (bindump_pair_elem carry head) tail in fold (true::true::true::[]))

let binload_pair_elem i j = function
	| false::b::stream				-> (* SS *)
		(i+1, j+1, SS b, stream)
	| true::false::true::stream		-> (* SP *)
		let invar, stream = binload_invar j stream in
		(i+1, j, SP invar, stream)
	| true::false::false::stream	-> (* PS *)
		let invar, stream = binload_invar i stream in
		(i, j+1, PS invar, stream)
	| true::true ::false::stream	-> (* PP *)
		let invarX, stream = binload_invar i stream in
		let invarY, stream = binload_invar j stream in
		(i, j, PP (invarX, invarY), stream)
	| _								->  assert false

let binload_pair = (
	let rec fold i j carry = function
		| [] -> carry
		| true::true ::true :: _ -> carry
		| stream ->
			let i, j, e, stream = binload_pair_elem i j stream in
			fold i j (e::carry) stream
	in fold 0 0 [])


let bindump_edge (b, l) = b::(bindump_uniq l) |> Array.of_list |> Bitv.L.of_bool_array 
let binload_edge = Bitv.L.to_bool_array >> Array.to_list >> (function b::l -> (b, binload_uniq l) | _ -> assert false)

let bindump_node (b, l) = b::(bindump_pair l) |> Array.of_list |> Bitv.L.of_bool_array
let binload_node = Bitv.L.to_bool_array >> Array.to_list >> (function b::l -> (b, binload_pair l) | _ -> assert false)

let bindump_node_and ((bx, by), l) = bx::by::(bindump_pair l) |> Array.of_list |> Bitv.L.of_bool_array
let binload_node_and = Bitv.L.to_bool_array >> Array.to_list >> (function bx::by::l -> ((bx, by), binload_pair l) | _ -> assert false)

let bindump_node_xor l = bindump_pair l |> Array.of_list |> Bitv.L.of_bool_array
let binload_node_xor = Bitv.L.to_bool_array >> Array.to_list >> binload_pair

let bindump_tacx (t, l) = TacxTypes.bindump_ttag t (bindump_pair l) |> Array.of_list |> Bitv.L.of_bool_array
let binload_tacx = Bitv.L.to_bool_array >> Array.to_list >> TacxTypes.binload_ttag >> (fun (t, l) -> (t, binload_pair l))

let expand_uniq : uniq -> uniq=
	let aux0 =
		let rec aux carry = function
			| [], [] -> List.rev carry
			| (S _)::x', y::y' -> aux (y::carry) (x', y')
			| (P _)::x', y' -> aux (false::carry) (x', y')
			| _ -> assert false
		in
		fun vect uniq -> Bitv.L.of_bool_list (aux [] (uniq, (Bitv.L.to_bool_list vect)))
	in
	let aux =
		let rec aux carry = function
			| [] -> List.rev carry
			| head::tail -> match head with
				| S _ | P (_, None) -> aux (head::carry) tail
				| P (b, Some i) -> aux ((P (b, Some(aux0 i tail)))::carry) tail
		in aux []
	in aux

let iuniq_of_riuniq : iuniq -> iuniq=
	let aux0 =
		let rec aux carry = function
			| [], [] -> List.rev carry
			| (IS _)::x', y::y' -> aux (y::carry) (x', y')
			| (IP _)::x', y' -> aux (false::carry) (x', y')
			| _ -> assert false
		in
		fun vect uniq -> aux [] (uniq, vect)
	in
	let aux =
		let rec aux carry = function
			| [] -> List.rev carry
			| head::tail -> match head with
				| IS _ | IP (_, None) -> aux (head::carry) tail
				| IP (b, Some i) -> aux ((IP (b, Some(aux0 i tail)))::carry) tail
		in aux []
	in aux

let add : bool list -> bool list -> bool list = List.map2 ( <> ) 
let cadd = function
	| true -> add
	| false -> fun x _ -> x


let xsev_vec_of_xiuniq b =
	let aux =
		let rec aux carry = function
			| [] -> List.rev ((None, b)::carry)
			| head::tail -> let head = match head with
				| IS b -> None, b
				| IP (b, opv) -> Some((match opv with None -> MyList.ntimes false (List.length tail) | Some v -> v)@[b]), false
				in aux (head::carry) tail
		in aux []
	in aux >> List.split

let xsev_rvec_of_xiuniq b =
	let aux =
		let rec aux carry = function
			| [] -> List.rev ((None, Some b)::carry)
			| head::tail -> let head = match head with
				| IS b -> None, Some b
				| IP (b, opv) -> Some((match opv with None -> MyList.ntimes false (List.length tail) | Some v -> v)@[b]), None
				in aux (head::carry) tail
		in aux []
	in aux >> List.split >> (fun (sev, opvec) -> (sev, MyList.list_of_oplist opvec))

let xsev_rvec_of_xuniq (b, uniq) =
	uniq |> xiuniq_of_xuniq |> iuniq_of_riuniq |> (xsev_rvec_of_xiuniq b)

let xiuniq_of_xsev_rvec sev vec =
	(* bsev + vect -> iuniq *)
	let sev, last = MyList.last sev in
	assert(last = None);
	let vec, b = MyList.last vec in
	let aux0 =	
		let rec aux some carry = function
			| []			-> assert false
			| [x]			-> x, (if some then Some(List.rev carry) else None)
			| head::tail	-> aux (head||some) (head::carry) tail
		in function
			| [] -> assert false
			| liste -> aux false [] liste
	in
	let aux1 =
		let rec aux carry = function
			| ([], []) -> List.rev carry
			| ([], _) -> assert false
			| (None::x', y::y') -> aux ((IS y)::carry) (x', y')
			| (None::_ , []) -> assert false
			| ((Some v)::x', y') -> aux ((IP(aux0 v))::carry) (x', y')
		in fun sev vec -> aux [] (sev, vec)
	in
	(b, aux1 sev vec)

let uniq_of_rsev_rvec sev vec = 
	let b, sev = xiuniq_of_xsev_rvec sev vec in
	(b, xuniq_of_xiuniq sev)

let riuniq_of_iuniq u =
	assert(iuniq_check u);
	let sev, vec = xsev_rvec_of_xiuniq false u in
	assert(List.length vec = Bsev.sev_count_none sev);
	assert(Bsev.check sev);
	let sev = Bsev.sev_reduce sev in
	assert(Bsev.check_reduce sev);
	assert(List.length vec = Bsev.sev_count_none sev);
	let b, u = xiuniq_of_xsev_rvec sev vec in
	assert(b = false);
	assert(riuniq_check u);
	u

let uniq_compose =
(*	let copadd = function
		| false -> fun _ x -> x
		| true	-> function
			| None		-> fun y -> y
			| Some x	-> (add x)
	in *)
	let aux0 =
		let rec aux carry = function
			| ([], [])				-> List.rev carry
			| ((IS _)::x', y::y')	-> aux (y::carry) (x', y')
			| ((IP _)::x', y')		-> aux (false::carry) (x', y')
			| _						-> assert false
		in fun v tail -> aux [] (tail, v)
	in
	let rec aux carry = function
		| ([], [], (bp, [])) -> (bp, List.rev carry)
		| ((IS bx)::x', y::y', (bp, p::p')) ->
		(
			let bx = bx <> p in
			match y with
			| IS by -> aux ((IS (bx<>by))::carry) (x', y', (bp, p'))
			| IP (by, vy) ->
				let bp = bp <> (bx && by) in
				match vy with
				| None -> aux ((IP(by, None))::carry) (x', y', (bp, p'))
				| Some iy -> 
					let iy = aux0 iy x' in
					assert(List.length p' = List.length iy);
					let p' = cadd bx p' iy in
					aux ((IP(by, Some iy))::carry) (x', y', (bp, p'))
			
		)
		| ((IP (bx, opx))::x', y', (bp, p::p')) ->
		(
			let bp = bp <> (p && bx) in
			let p' = match opx with
				| None -> p'
				| Some v -> cadd p p' v in
			aux ((IP(bx, opx))::carry) (x', y', (bp, p'))
		)
		| _ -> assert false
	in
	fun uniqC uniq ->
		let uniqC = uniqC |> xiuniq_of_xuniq |> iuniq_of_riuniq in
		let uniq  = uniq  |> xiuniq_of_xuniq |> iuniq_of_riuniq in
		assert(List.length uniq = count_is uniqC);
		let b, uniqX = aux [] (uniqC, uniq, (false, MyList.ntimes false (List.length uniqC))) in
		(b, uniqX |> riuniq_of_iuniq |> xuniq_of_xiuniq)


let compose_edge (bC, uC) (b, u) =
	let b', u = uniq_compose uC u in
	(bC<>b<>b', u)

let compose eC (e, i) = (compose_edge eC e, i)


let pair_recompose sev uniqX uniqY vec =
	let sev, last = MyList.last sev in
	assert(last = None);
	let n = List.length sev in
	assert(n = List.length uniqX);
	assert(n = List.length uniqY);
	let mix = List.combine sev (List.combine uniqX uniqY) in
	let f = fun (s, (x, y)) -> match s with
		| None -> Some (match x, y with
			| S _, S _ -> SS false
			| P x, S _ -> PS x
			| S _, P y -> SP y
			| P x, P y -> PP (x, y))
		| Some _ -> None
	in
	let sev = MyList.opmap f mix in
	List.map2 (fun s v -> match s with
		| SS _	-> SS v
		| x		-> assert(v = false); x) sev vec

let uniq_recompose sev vec =
	(* bsev + vect -> iuniq *)
	let sev, last = MyList.last sev in
	assert(last = None);
	(*MAYBE: let vec, b = MyList.last vec in*)
	let aux0 =	
		let rec aux some carry = function
			| []			-> assert false
			| [x]			-> x, (if some then Some(List.rev carry) else None)
			| head::tail	-> aux (head||some) (head::carry) tail
		in function
			| [] -> assert false
			| liste -> aux false [] liste
	in
	let rec aux carry = function
		| ([], []) -> List.rev carry
		| (None::x', y::y') -> aux ((IS y)::carry) (x', y')
		| ((Some v)::x', y') -> aux ((IP(aux0 v))::carry) (x', y')
		| _ -> assert false
	in
	let iuniq = aux [] (sev, vec) in
	(* iuniq -> uniq *)
	xuniq_of_xiuniq iuniq
		

let sev_vec_of_uniq (b, u) = xsev_vec_of_xiuniq b (u |> xiuniq_of_xuniq |> iuniq_of_riuniq)


let pair_split pair = List.map (function
	| SS s		-> S false, S s
	| PS x		-> P x, S false
	| SP y		-> S false, P y
	| PP(x, y)	-> P x, P y) pair |> List.split

let node_split (b, p) =
	let uX, uY = pair_split p in
	(false, uX), (b, uY)

let solve_cons_1 ((_, uniqX) as x) ((_, uniqY) as y) =
	let (sx, vx) as dx = sev_vec_of_uniq x
	and (sy, vy) as dy = sev_vec_of_uniq y in
	assert(Bsev.check sx);
	assert(Bsev.check sy);
	let f, sev, ((bY, vY), (bXY, vXY)) = Bsev.sev_inter_cons dx dy in
	assert(List.length sev = List.length uniqX + 1);
	assert(List.length sev = List.length uniqY + 1);
	let uniqX, uniqY = Tools.cswap f (uniqX, uniqY) in
	let xy  = pair_recompose sev uniqX uniqY vY in
	let vXY = uniq_recompose sev vXY in
	(* start checks *)
	let eX, eY = node_split (bY, xy) in
	let e = (bXY, vXY) in
	let eX' = compose_edge e eX
	and eY' = compose_edge e eY in
	let eX', eY' = Tools.cswap f (eX', eY') in
	assert((eX' = x)&&(eY' = y));
	(* end checks *)
	(f, (bY, xy), (bXY, vXY))

let solve_and_1 ((bX, uniqX) as x) ((bY, uniqY) as y) =
	let (sx, vx) as dx = sev_vec_of_uniq x
	and (sy, vy) as dy = sev_vec_of_uniq y in
	assert(Bsev.check sx);
	assert(Bsev.check sy);
	let f, sev, (bX, (bY, vY), (bXY, vXY)) = Bsev.sev_inter_and dx dy in
	let uniqX, uniqY = Tools.cswap f (uniqX, uniqY) in
	let xy  = pair_recompose sev uniqX uniqY vY in
	let vXY = uniq_recompose sev vXY in
	(* start checks *)
	let uX, uY = pair_split xy in
	let eX = (bX, uX)
	and eY = (bY, uY) in
	let e = (bXY, vXY) in
	let eX' = compose_edge e eX
	and eY' = compose_edge e eY in
	let eX', eY' = Tools.cswap f (eX', eY') in
	assert((eX' = x)&&(eY' = y));
	(* end checks *)
	(f, (bX, bY, xy), (bXY, vXY))

let solve_xor_1 ((bX, uniqX) as x) ((bY, uniqY) as y) =
	let (sx, vx) as dx = sev_vec_of_uniq x
	and (sy, vy) as dy = sev_vec_of_uniq y in
	assert(Bsev.check sx);
	assert(Bsev.check sy);
	let f, sev, (vY, (bXY, vXY)) = Bsev.sev_inter_xor dx dy in
	let uniqX, uniqY = Tools.cswap f (uniqX, uniqY) in
	let xy  = pair_recompose sev uniqX uniqY vY in
	let vXY = uniq_recompose sev vXY in
	(f, xy, (bXY, vXY))

let solve_cons_0 (bX, uniqX) (bY, uniqY) =
	let aux =
		let rec aux carryY carryXY = function
			| [] -> Some(List.rev carryY, List.rev carryXY)
			| head::tail -> match head with
				| P x, P y -> if x = y
					then aux carryY ((P x)::carryXY) tail
					else None
				| S x, S y -> aux ((x<>y)::carryY) ((S x)::carryXY) tail
				| _ -> None
		in aux [] []
	in
	if uniqX = uniqY
	then Some(bX, (P(bX<>bY, None))::uniqX)
	else match aux (List.combine uniqX uniqY) with
	| None -> None
	| Some(diff, uniqXY) -> Some (bX, (P (invar_of_iinvar (bX<>bY, Some diff))::uniqX))

let size (b, u) = List.length u

let solve_cons getid ((eX, iX) as x) ((eY, iY) as y)=
	assert(size eX = size eY);
	let cmp = CpGops.cmpid getid (iX, iY) in
	match (if(cmp = None)
		then solve_cons_0 eX eY
		else None) with
	| Some e	-> Utils.MEdge (e, iX)
	| None		->
		let f = match cmp with Some true -> true | _ -> false in
		let (eX, iX), (eY, iY) = Tools.cswap f (x, y) in
		let f', (bY, xy), (bXY, vXY) = solve_cons_1 eX eY in
		let iX, iY = Tools.cswap f' (iX, iY) in
		Utils.MNode ((bXY, (S(f<>f'))::vXY), ((bY, xy), iX, iY))

let node_push_cons gid x y = match solve_cons gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (e', x, y)) -> Utils.MNode (e, (bindump_node e', x, y))

let tacx_push_cons gid x y = match solve_cons gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, ((b, l), x, y)) -> Utils.MNode (e, (bindump_tacx (TacxTypes.TCons b, l), x, y))

let get_root_n b n = ((b, MyList.ntimes (P(false, None)) n), Utils.Leaf ())

let get_root b ((_, l), _) = get_root_n b (List.length l)

let uniq_is_root = List.for_all (function P(false, None) -> true | _ -> false)

let deco_is_root (b, u) = if uniq_is_root u
	then Some b
	else None

let edge_is_root (e, i) = match i with
	| Utils.Leaf () -> deco_is_root e
	| Utils.Node _  -> None

let pair_is_root : pair -> bool * bool=
	let isP = function
		| (false, None) -> true
		| _				-> false
	in
	let rec aux rX rY = function
		| [] -> (rX, rY)
		| head::tail -> match head with
			| SS _ -> (false, false)
			| PS x -> aux (rX&&(isP x)) false tail
			| SP y -> aux false (rY&&(isP y)) tail
			| PP (x, y) -> aux (rX&&(isP x)) (rY&&(isP y)) tail
	in aux true true

let uniqX_of_pair = List.map (function
	| SS _
	| SP _		-> S false
	| PS x
	| PP(x, _)	-> P x)

let uniqY_of_pair = List.map (function
	| SS b		-> S b
	| PS _		-> S false
	| SP y
	| PP(_, y)	-> P y)


let node_pull_node _ (c, ix, iy) =
	let ex, ey = node_split (binload_node c) in
	(ex, ix), (ey, iy)

let apply_reduced_phase =
	let rec aux carry = function
		| ([], []) -> List.rev carry
		| ((S x)::x', y::y') -> aux ((S(x<>y))::carry) (x', y')
		| ((P x)::x', y') -> aux ((P x)::carry) (x', y')
		| _ -> assert false
	in fun p u -> aux [] (u, p)

let node_pull getid ((bx, lx), i) = match lx with
	| [] -> assert false
	| h::lx' -> let e' = (bx, lx') in match h with
		| S b -> Utils.MNode (fun node ->
			let x', y' = node_pull_node getid node in
			let x', y' = Tools.cswap b (x', y') in
			(compose e' x'), (compose e' y'))
		| P(b, opv) -> Utils.MEdge (match opv with
			| None -> ((bx, lx'), i), ((bx<>b, lx'), i)
			| Some v ->
			(
				let lx'' = apply_reduced_phase (Bitv.L.to_bool_list v) lx' in
				((bx, lx'), i), ((bx<>b, lx''), i)
			))

let tacx_split (t, lxy) =
	let lx, ly = pair_split lxy in
	match t with
	| TacxTypes.TAnd (bx, by) -> (TacxTypes.And,  (bx, lx), (by, ly))
	| TacxTypes.TCons by	  -> (TacxTypes.Cons, (false, lx), (by, ly))
	| TacxTypes.TXor		  -> (TacxTypes.Xor,  (false, lx), (false, ly))

let tacx_pull_node _ (c, ix, iy) =
	let t, ex, ey = tacx_split (binload_tacx c) in
	(t, (ex, ix), (ey, iy))

let tacx_pull (getid:'t -> 'i) (((bx, lx), i):'t edge) = match lx with
	| [] -> assert false
	| h::lx' -> match h with
		| S b -> Utils.MNode (fun node ->
			let t, x', y' = tacx_pull_node getid node in
			let e' = match t with
				| TacxTypes.Cons	-> (bx, lx')
				| _		-> (bx, lx )
			in
			let a, b = Tools.cswap b ((compose e' x'), (compose e' y')) in
			(t, a, b))
		| P(b, opv) -> Utils.MEdge (match opv with
			| None -> TacxTypes.Cons, ((bx, lx'), i), ((bx<>b, lx'), i)
			| Some v ->
			(
				let lx'' = apply_reduced_phase (Bitv.L.to_bool_list v) lx' in
				TacxTypes.Cons, ((bx, lx'), i), ((bx<>b, lx''), i)
			))

let uniqY_of_pair = List.map (function
	| SS b		-> S b
	| PS _		-> S false
	| SP y
	| PP(_, y)	-> P y)

let solve_and gid (eX, iX) (eY, iY) =
	let f = match CpGops.cmpid gid (iX, iY) with Some true -> true | _ -> true in
	let (eX, iX), (eY, iY) = Tools.cswap f ((eX, iX), (eY, iY)) in
	let f', (bX, bY, xy), (bXY, vXY) = solve_and_1 eX eY in
	let iX, iY = Tools.cswap f' (iX, iY) in
	let n = List.length xy in
	match (match pair_is_root xy with
		| true, true	-> Some (get_root_n (bX&&bY) n)
		| true, false	-> Some (if bX
			then ((bY, uniqY_of_pair xy), iY)
			else (get_root_n false n))
		| false, true	-> Some (if bY
			then ((bX, uniqX_of_pair xy), iX)
			else (get_root_n false n))
		| false, false	-> None) with
	| Some e -> Utils.MEdge (compose (bXY, vXY) e)
	| None -> Utils.MNode ((bXY, vXY), (((bX, bY), xy), iX, iY))

let tacx_push_and gid x y = match solve_and gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (((bx, by), lxy), x, y)) -> Utils.MNode (e, (bindump_tacx (TacxTypes.TAnd (bx, by), lxy), x, y))

let node_solve_and : ('t -> 'i) -> 't edge * 't edge -> ('t edge, edge_state * (Bitv.t * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode)) Utils.merge =
	fun gid (x, y) -> match solve_and gid x y with
		| Utils.MEdge e -> Utils.MEdge e
		| Utils.MNode (e, (e', x, y)) -> Utils.MNode (e, (bindump_node_and e', x, y))



let solve_xor gid (eX, iX) (eY, iY) =
	let f, (eX, iX), (eY, iY) = if (match CpGops.cmpid gid (iX , iY) with Some true -> false | _ -> true)
		then (false, (eX, iX), (eY, iY))
		else (true,  (eY, iY), (eX, iX)) in
	let f', xy, (bXY, vXY) = solve_xor_1 eX eY in
	let iX, iY = if f' then iY, iX else iX, iY in
	let n = List.length xy in
	match (match pair_is_root xy with
		| true, true	-> Some (get_root_n false n)
		| true, false	-> Some ((false, uniqY_of_pair xy), iY)
		| false, true	-> Some ((false, uniqX_of_pair xy), iX)
		| false, false	-> None) with
	| Some e -> Utils.MEdge (compose (bXY, vXY) e)
	| None -> Utils.MNode ((bXY, vXY), (xy, iX, iY))

let tacx_push_xor gid x y = match solve_xor gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (l, x, y)) -> Utils.MNode (e, (bindump_tacx (TacxTypes.TXor, l), x, y))

let node_solve_xor gid (x, y) = match solve_xor gid x y with
	| Utils.MEdge e -> Utils.MEdge e
	| Utils.MNode (e, (l, x, y)) -> Utils.MNode (e, (bindump_node_xor l, x, y))

let tacx_push gid = function
	| TacxTypes.And  -> tacx_push_and  gid
	| TacxTypes.Cons -> tacx_push_cons gid
	| TacxTypes.Xor  -> tacx_push_xor  gid
