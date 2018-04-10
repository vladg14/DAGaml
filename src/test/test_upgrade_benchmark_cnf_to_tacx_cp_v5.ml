let file_in = Sys.argv.(1)
and file_out = Sys.argv.(2);;

open StrLoadCnf
open IoUtils
open IterExtra

(* STEP 1: verilog parsing [START]*)
let my_cnf = load (Stream.of_channel (open_in file_in));;

(* STEP 1: CNF parsing [DONE] *)


(* STEP 2: from CNF to Cp.TACX *)

type tree = Const of bool | Node of tree * tree * tree

type term = bool * int (* --> int * bool *)
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


let merge_singles lX lY =
	let lXY = MyList.merge_uniq lX lY in
	let rec aux = function
		| [] | [_] -> true
		| (bX, iX)::((bY, iY)::_ as tail) -> (iX<>iY)&&(aux tail)
	in
	if aux lXY then (Ok lXY) else (Error())
	

let simplify xmax llist =
	let apply_singles singles llist = MyList.opmap (setlist singles') llist in
	let return_1 singles llist = (List.map (fun x -> [x]) singles)@llist
	and return_2 couples llist = (List.map (fun (x, y) -> [x; y]) couples)@llist in
	let rec aux1 singles llist =
		if List.exists ((=)[]) llist then (Error false) else(
		let llist, singles' = MyList.partition (function [x] -> Ok x | l -> Error l) llist in
		if singles' = [] then (Ok (return_1 singles llist))
		else
		(
			let singles' = List.sort_uniq Pervasives.compare singles' in
			match merge singles singles' with
			| Error () -> Error false
			| Ok singles -> aux singles (apply_singles singles' llist)
		))
	in
	let empty_graph2 () =
		let dump_node (b, i) = 2*i+(if b then 1 else 0)
		and load_node x      = (x mod 2 = 1, x/2) in
		GraphAA.make dump_node load_node (2*(xmax+1))
	in
	let rec aux2 graph llist =
		assert(List.for_all (fun clause -> List.length clause >= 2) llist);
		let llist, couples' = MyList.partition (function [x; y] -> Ok (x, y) | l -> Error l) llist in
		if couples' = [] then (Ok (return_2 couples llist))
		else
		(
			let set nX nY = GraphAA.o3set graph (nX, nY)
			and get nX nY = GraphAA.o3get graph (nX, nY)
			and get_bidir nX nY = GraphAA.o3get_bidir graph (nX, nY) in
			let neg (b, i) = (not b, i) in
			List.iter (fun (nX, nY) -> set (neg nX) nY; set nX (neg nY)) couples';
			let graph = GraphAA.closure graph in
			(* STEP 1 : false *)
			let funmap i = get_bidir (false, i) (true, i) in
			let each = range 0 (xmax+1) in
			if each $$ funmap |> Iter.exists
			then (Error false) else (
			(* STEP 2 : constant *)
			let funmap (i, j) =
				let aux b = (get(false, i)(b, j))&&(get(true, i)(b, j)) in
				if      aux false then Some (false, j)
				else if aux true  then Some (true , j)
				else                   None
			in
			let singles = each $* each $? funmap |> Iter.to_list in
			if not(singles = [])
			then (wrap2 singles (apply_singles singles (return_2 couples llist)))
			else
			(
			(* STEP 3 : equality *)
			(* STEP 4 : clause reduction *)
			))
		)
	and wrap2 singles llist = match aux1 singles llist with
		| Ok llist -> (aux2 (empty_graph2()) llist)
		| Error re -> Error re
	in
	match llist with
	| [] -> Error true
	| llist -> wrap2 [] llist


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
