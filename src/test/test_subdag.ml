
module UnitH : Subdag.MODELE with
		type node = unit
	and	type edge = unit
	and type leaf = unit
=
struct
	
	type node = unit
	type edge = unit
	type leaf = unit

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push _ (((), nx):'t e) (((), ny): 't e) = Utils.MNode ((():edge), (((), nx, ny):'t n))
	let pull _ (((), nx):'t e) = match nx with
		| Utils.Leaf ()	-> assert false
		| Utils.Node n	-> Utils.MNode (fun (((), nx, ny):'t n) -> (((), nx):'t e), (((), ny): 't e))
	let pull_node _ (((), nx, ny):'t n) = ((((), nx):'t e), (((), ny):'t e))
	let compose  (():edge) (x:'t e)  = x


	let dump_node = Some (fun () -> Tree.Node [])
	let load_node = Some (function Tree.Node [] -> (), () | _ -> assert false)
	let dot_of_node = None

	let dump_edge = Some STD.unit
	let load_edge = Some STL.unit
	let dot_of_edge = None

	let dump_leaf = Some (fun () -> Tree.Node [])
	let load_leaf = Some (function Tree.Node [] -> ((), Utils.Leaf ()) | _ -> assert false)
	let dot_of_leaf = None

end;;

(*
module UnitH : Subdag.MODELE with
		type node = unit
	and	type edge = unit
	and type leaf = unit
=
struct
	
	type node = unit
	type edge = unit
	type leaf = unit

	type 't gn = (leaf, 't) Utils.gnode
	type 't n = node * 't gn * 't gn	
	type 't e = edge * 't gn

	let push (((), nx):'t e) (((), ny): 't e) = Utils.MNode ((():edge), (((), nx, ny):'t n))
	let pull (((), nx):'t e) = match nx with
		| Utils.Leaf ()	-> assert false
		| Utils.Node n	-> Utils.MNode (fun (((), nx, ny):'t n) -> (((), nx):'t e), (((), ny): 't e))
	let pull_node (((), nx, ny):'t n) = ((((), nx):'t e), (((), ny):'t e))
	let compose (():edge) (x:'t e)  = x


	let dump_node = Some (fun () -> Tree.Node [])
	let load_node = Some (function Tree.Node [] -> (), () | _ -> assert false)
	let dot_of_node = None

	let dump_edge = Some STD.unit
	let load_edge = Some STL.unit
	let dot_of_edge = None

	let dump_leaf = Some (fun () -> Tree.Node [])
	let load_leaf = Some (function Tree.Node [] -> ((), Utils.Leaf ()) | _ -> assert false)
	let dot_of_leaf = None

end;;
*)
module T = Subdag.MODULE(UnitH);;

let man = T.newman();;

let x = T.push_leaf (():UnitH.edge) (():UnitH.edge);;
let y = T.push man x x;;
let z = T.push man x y;;
let z' = T.push man y x;;

assert(T.pull man z' = (y, x));;
assert(T.pull man z  = (x, y));;
assert(T.pull man y  = (x, x));;
assert(x = ((), Utils.Leaf ()));;

module IBA_M : T.MODELE_IBOP =
struct
	type t = unit
	let compare () () = 0
	type transform = unit
	let compose () x = x
	let decomp x y () = (((), x), ((), y))
	let solver _ = function
		| (((), Utils.Leaf ()):T.edge), ((), y)
		| ((), y), (((), Utils.Leaf ()):T.edge) -> Utils.MEdge ((), y)
		| ((), x), ((), y) -> Utils.MNode ((), ((), x, y))
end;;

module IBA = T.IBOP(IBA_M);;

let iba_man, iba = IBA.newman man;;
let xy = iba x y;;
let yy = iba y y;;
STree.pprint [IBA.dump_stats iba_man];;

exit 0;;
