let file_in = Sys.argv.(1)
and file_out = Sys.argv.(2)
and force_cst = (try int_of_string(Sys.argv.(3)) with _ -> 1);;

open StrLoadCnf
open IoUtils

(* STEP 1: verilog parsing [START]*)
let my_cnf = load (Stream.of_channel (open_in file_in));;

(* STEP 1: CNF parsing [DONE] *)

type tree = Const of bool | Node of tree * tree * tree

type term = bool * int
type clause = term list
type formule = clause list

(* STEP 2.0 : find a better variable order *)

type int_o3 = (int array) (* var -> idx *) * (int array) (* idx -> var *)

type aclause  = int array
type aformule = aclause array (* len = |formule| *)
type rformule = int list array (* list of clauses to which the term belong, len = |var| *)

type preproc = int_o3 * aformule * rformule

let force_preproc n (formule:formule) : preproc =
	let rformule = Array.make n [] in
	let id = Array.init n (fun i -> i) in
	let o3 = (id, id) in
	let aformule = Array.mapi (fun idx clause ->
		let aclause = Array.map snd (Array.of_list clause) in
		Array.iter (fun var -> rformule.(var) <- idx::(rformule.(var))) aclause;
		aclause) (Array.of_list formule)
	in
	(o3, aformule, rformule)

let force_calc_cog ((o3, af, rf):preproc) =
	Array.map (fun ac ->
		let sum = Array.fold_left (fun sum var -> sum+(fst o3).(var)) 0 ac in
		sum/(Array.length ac)) af

let force_calc_l' n ((o3, af, rf) as state:preproc) =
	let cog = force_calc_cog state in
	let temp = Array.make n [] in
	for var = 0 to n-1
	do
		let sum = List.fold_left (fun sum idx -> sum+cog.(idx)) 0 rf.(var) in
		let idx = sum / (List.length rf.(var)) in
		temp.(idx) <- (var::(temp.(idx)));
	done;
	let stack = ref [] in
	let push x = stack:=(x::(!stack)) in
	let pull () = List.rev !stack |> Array.of_list in
	Array.iter (List.iter push) temp;
	let fst_o3 = pull() in
	let snd_o3 = Array.make n 0 in
	Array.iteri (fun var idx -> snd_o3.(idx) <- var) fst_o3;
	((fst_o3, snd_o3), af, rf)

let force_apply n (formule:formule) : string list * formule =
	if n = 0 then ([], (match formule with [] -> [] | _ -> [[]]))
	else (
		assert(n<>0); (* strange case to avoid, could find a meaning *)
		let state : preproc ref = ref(force_preproc n formule) in
		for i = 0 to force_cst * ((Tools.math_log 2 n)+1)
		do state:=(force_calc_l' n !state) done;
		let (var_to_idx, idx_to_var), _, _ = !state in
		let inputs = List.map (fun var -> "x"^(string_of_int var)) (Array.to_list idx_to_var) in
		let formule = List.map (List.map (fun (neg, var) -> (neg, var_to_idx.(var)))) formule in
		(inputs, formule)
	)

(* STEP 2: from CNF to Cp.TACX *)


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
	let ( *! ) = Cp.( *! ) man
	and ( &! ) = Cp.( &! ) man in
	let rec map_llist x llist =	
		assert(x <= xmax);
		match simplify llist with
		| Error bool -> Cp.make_const bool (xmax-x)
		| Ok llist ->
			Oops.list_et (&!) (List.map (rec_llist x) (andsplit xmax llist))
	and     rec_llist x llist =
		let if0, if1, ifX = split3 x llist in
		let if0 = map_llist (x+1) if0
		and if1 = map_llist (x+1) if1
		and ifX = map_llist (x+1) ifX in
		(Cp.no ((Cp.no if1) *! (Cp.no if0))) &! (Cp.push_pass ifX)
	in map_llist 0 llist


let upgrade mymodule =
	let tacx_input, formule = force_apply mymodule.input mymodule.clauses in
	let man = Cp.TACX.newman() in
	{
		tacx_name = "CNF";
		tacx_man  = man;
		tacx_input;
		tacx_edges= ["OUT", treefy man mymodule.input formule]
	}
;;

let my_tacx = upgrade my_cnf;;

Cp.TACX.dumpfile my_tacx.tacx_man (List.map (fun (name, edge) -> edge) my_tacx.tacx_edges) file_out;;

exit 0;;
