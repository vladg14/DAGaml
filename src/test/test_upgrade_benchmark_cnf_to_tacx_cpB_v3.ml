let file_in = Sys.argv.(1)
and file_out = Sys.argv.(2);;

open StrLoadCnf
open IoUtils

(* STEP 1: verilog parsing [START]*)
let my_cnf = load (Stream.of_channel (open_in file_in));;

(* STEP 1: CNF parsing [DONE] *)


(* STEP 2: from CNF to Cp.TACX *)

type tree = Const of bool | Node of tree * tree * tree

type term = bool * int
type clause = term list
type formule = clause list

let set ((b, x) : term) : clause -> clause option =
	let rec aux carry = function
		| [] -> Some (List.rev carry)
		| (b', y)::tail when x = y -> if b = b' then None else (aux carry tail)
		| head::tail -> aux (head::carry) tail
	in aux []

let setlist liste clause = List.fold_left (function
	| None -> (fun _ -> None)
	| Some c -> (fun x -> set x c)
) (Some clause) liste 


let merge lX lY =
	let lXY = MyList.merge_uniq lX lY in
	let rec aux = function
		| [] | [_] -> true
		| (bX, iX)::((bY, iY)::_ as tail) -> (iX<>iY)&&(aux tail)
	in
	if aux lXY then (Ok lXY) else (Error())
	

let simplify llist =
	let rec aux singles llist =
		if List.exists ((=)[]) llist then (Error false) else(
		let llist, singles' = MyList.partition (function [x] -> Ok x | l -> Error l) llist in
		if singles' = [] then (Ok ((List.map (fun x -> [x]) singles)@llist))
		else
		(
			let singles' = List.sort_uniq Pervasives.compare singles' in
			match merge singles singles' with
			| Error () -> Error false
			| Ok singles -> aux singles (MyList.opmap (setlist singles') llist)
		))
	in match llist with
	| [] -> Error true
	| llist -> aux [] llist


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

let andsplit xmax llist =
	let uf = UnionFind.init xmax in
	List.iter (function
		| [] -> assert false
		| (_, i)::tail -> ignore(List.map (fun (_, j) -> ignore(UnionFind.union uf i j)) tail)
	) llist;
	let remap, _ = UnionFind.ends uf in
	let split = Array.make (Array.length remap) [] in
	let push x y = split.(x) <- y::split.(x) in
	List.iter (function
		| [] -> assert false
		| ((_, i)::tail) as clause -> push remap.(i) clause) llist;
	let parts = List.filter (function [] -> false | _ -> true) (Array.to_list split) in
(*	if List.length parts > 1
	then
	(
		List.iter (fun l -> print_int (List.length l); print_string " ") parts;
		print_newline();
	);*)
	parts



let treefy man xmax llist =
	let ( *! ) = CpB.( *! ) man
	and ( &! ) = CpB.( &! ) man
	and no     = CpB.neg
	and push_P = CpB.push_pass  in
	let rec map_llist x llist =	
		assert(x <= xmax);
		match simplify llist with
		| Error bool -> Cp.make_const bool (xmax-x)
		| Ok llist ->
			OopsB.list_et (&!) (List.map (rec_llist x) (andsplit xmax llist))
	and     rec_llist x llist =
		let if0, if1, ifX = split3 x llist in
		let if0 = map_llist (x+1) if0
		and if1 = map_llist (x+1) if1
		and ifX = map_llist (x+1) ifX in
		(no ((no if1) *! (no if0))) &! (push_P ifX)
	in map_llist 0 llist


let upgrade mymodule =
	let man = CpB.TACX.G1.newman() in
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
