open CnfTypes
open IoUtils
open Expr

(* type tree = Const of bool | Node of tree * tree * tree *)

type term = bool * int
type clause = term list
type formule = clause list

let conv_cnf_to_mexpr cnf args =
	let set ((b, x) : term) : clause -> clause option =
		let rec aux carry = function
			| [] -> Some (List.rev carry)
			| (b', y)::tail when x = y -> if b = b' then None else (aux carry tail)
			| head::tail -> aux (head::carry) tail
		in aux []
	in

	let setlist liste clause = List.fold_left (function
		| None -> (fun _ -> None)
		| Some c -> (fun x -> set x c)
	) (Some clause) liste 
	in

	let merge lX lY =
		let lXY = MyList.merge_uniq lX lY in
		let rec aux = function
			| [] | [_] -> true
			| (bX, iX)::((bY, iY)::_ as tail) -> (iX<>iY)&&(aux tail)
		in
		if aux lXY then (Ok lXY) else (Error())
	in

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
	in

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
	in

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
		parts
	in

	let string_of_var x = "x"^(string_of_int x) in

	let treefy xmax llist =
		let rec map_llist x llist =	
			assert(x <= xmax);
			match simplify llist with
			| Error bool -> PCst bool
			| Ok llist ->
				Tools.tree_of_list (&!) (List.map (rec_llist x) (andsplit xmax llist))
		and     rec_llist x llist =
			let if0, if1, ifX = split3 x llist in
			let if0 = map_llist (x+1) if0
			and if1 = map_llist (x+1) if1
			and ifX = map_llist (x+1) ifX in
			(no (cons (PVar (string_of_var x)) (no if1) (no if0))) &! ifX
		in map_llist 0 llist
	in

	let expr_input = MyList.init cnf.CnfTypes.input (fun x -> "x"^(string_of_int x)) in
	let expr_output = ["out"] in
	IoUtils.{
		expr_name = "cnf";
		expr_param = expr_input @ expr_output;
		expr_input;
		expr_output;
		expr_wire = [];
		expr_assign = [("out", treefy cnf.input cnf.clauses)];
	}
