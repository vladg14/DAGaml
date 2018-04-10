let file_in = Sys.argv.(1)
and file_out = Sys.argv.(2);;

open StrLoadCnf
open IoUtils

(* STEP 1: verilog parsing [START]*)
let my_cnf = load (Stream.of_channel (open_in file_in));;

(* STEP 1: CNF parsing [DONE] *)


(* STEP 2: from CNF to Cp.TACX *)

type tree = Const of bool | Node of tree * tree * tree

let set ((b, x):bool * int) =
	let rec aux carry = function
		| [] -> Some (List.rev carry)
		| (b', y)::tail when x = y -> if b = b' then None else (aux carry tail)
		| head::tail -> aux (head::carry) tail
	in aux []


let simplify llist =
	let rec aux c1 c2 cN = function
		| [] -> Ok ((List.rev c1)@(List.rev c2)@(List.rev cN))
		| []::tail -> Error false
		| [x]::tail -> aux ([x]::c1) c2 cN (MyList.opmap (set x) tail) 
(*		| [x]::tail -> aux ([x]::c1) c2 cN tail *)
		| [x; y]::tail -> aux c1 ([x; y]::c2) cN tail
		| head::tail -> aux c1 c2 (head::cN) tail
	in match llist with
	| [] -> Error true
	| llist -> aux [] [] [] llist

let split3 x =
	let where =
		let rec aux carry = function
			| [] -> (None, List.rev carry)
			| (b, y)::tail when x = y -> (Some b, (List.rev carry)@tail)
			| head::tail -> aux (head::carry) tail
		in aux []
	in
	let rec aux c0 c1 cX = function
		| [] -> (c0, c1, cX)
		| head::tail -> let sel, head' = where head in match sel with
			| Some false -> aux (head'::c0) c1 cX tail
			| Some true  -> aux c0 (head'::c1) cX tail
			| None       -> aux c0 c1 (head'::cX) tail
	in aux [] [] []


let init_union_find n = Array.init n (fun x -> x)

let rec find array x =
	if array.(x) = x
	then x
	else
	(
		let x' = find array (array.(x)) in
		array.(x) <- x';
		x'
	)

let union array x y =
	let x' = find array x
	and y' = find array y in
	if x' < y'
	then array.(x') <- y'
	else if y' < x'
	then array.(y') <- x'
	else ()

(*

let andsplit xmax llist =
	let array = init_union_find xmax in
	List.iter (function [] -> assert false | x::tail -> List.iter (union array x) tail) llist ;
	let count = Array.make xmax 0 in
	Array.iter (fun i -> count.(i) <- count.(i)+1) array;
	let coupe = Array.init xmax (fun _ -> []) in
	List.iter (function [] -> assert false | (x::_) as clause -> (let x' = find array x in coupe.(x') <- clause::(coupe.(x')))) llist;
	let return = ref [] in
	Array.iter (function [] -> () | list -> return := (list::(!return))) coupe;
	List.map List.rev !return;

*)

let treefy man xmax llist =
	let ( *! ) = Cp.( *! ) man
	and ( &! ) = Cp.( &! ) man in
	let rec recsplit3 x llist =	
		assert(x <= xmax);
		match simplify llist with
		| Error bool -> Cp.make_const bool (xmax-x)
		| Ok llist ->
		let if0, if1, ifX = split3 x llist in
		let if0 = recsplit3 (x+1) if0
		and if1 = recsplit3 (x+1) if1
		and ifX = recsplit3 (x+1) ifX in
		(Cp.no ((Cp.no if1) *! (Cp.no if0))) &! (Cp.push_pass ifX)
	in recsplit3 0 llist


let upgrade mymodule =
	let man = Cp.TACX.newman() in
	{
		tacx_name = "CNF";
		tacx_man  = man;
		tacx_input= MyList.init mymodule.input string_of_int;
		tacx_edges= ["OUT", treefy man mymodule.input mymodule.clauses]
	}
;;

let my_tacx = upgrade my_cnf;;

Cp.TACX.dumpfile my_tacx.tacx_man (List.map (fun (name, edge) -> edge) my_tacx.tacx_edges) file_out;;

exit 0;;
