(* UDAG : Unique - Directed Acyclic Diagramm (DAG)
 *
 *)

(* Generic typing of a DAG's DATA:
 * A DAG's type is described as the combination of nodes, edges and leafs
 *
 *)
module type GRAPH_HEADER = sig
	(* type of data on leafs *)
    type leaf
	(* type of data on edges *)
    type edge
	(* type of data on nodes *)
    type node
end

(* Generic typing of a UDAG
 * A UDAG has two methods:
 *   - push : create a new node (if necessarry) from its description and returns its unique identifier
 *   - pull : return a node's descritpion from its identifier
 *)
module type GRAPH = sig
    include GRAPH_HEADER
	(* identifier's type *)
    type ident

	(* represent either a leaf or a node identifier *)
    type next_t = (leaf, ident) Utils.gnode
	(* represent an edge *)
    type edge_t = edge * next_t
	(* represent a node *)
    type node_t = node * (edge_t list)
    
    type manager
    
	(* create a new manager with some default parameters *)
    val newman : unit -> manager
	(* manager method : create (only if necessarry) a new node in the structure, returns its identifier *)
    val push : manager -> node_t -> ident
	(* manager method : return the node associated to this identifier
		TODO: catch and emit exception if not found *)
    val pull : manager -> ident -> node_t
    
end

(* A more evolved representation of a DAG allowing to load/dump/export to dot a DAG by just doing so for its component *)
module type UDAG_HEADER = sig
    include GRAPH_HEADER

    val dump_leaf : (leaf -> StrTree.tree) option
    val load_leaf : (StrTree.tree -> leaf) option
    val dot_of_leaf : (leaf -> string) option

    val dump_edge : (edge -> StrTree.tree) option
    val load_edge : (StrTree.tree -> edge) option
    val dot_of_edge : (edge -> string) option
    
    val dump_node : (node -> StrTree.tree) option
    val load_node : (StrTree.tree -> node) option
    val dot_of_node : (int -> node -> string) option

end

module UDAG (Header:UDAG_HEADER) =
struct
    type ident = int

    type leaf = Header.leaf
    type edge = Header.edge
    type node = Header.node

    type next_t = (leaf, ident) Utils.gnode
    type edge_t = edge * next_t
    type node_t = node * (edge_t list)
    
    type manager = {
        unique : node_t H2Table.t;
    }

    
    let makeman hsize = {
        unique = H2Table.create hsize 0;
    }

    let default_newman_hsize = 10000
    let newman () = makeman default_newman_hsize
    
    let push udag = H2Table.push udag.unique
    let pull udag = H2Table.pull udag.unique
    
    let length udag = H2Table.length udag.unique

    let dump_stats udag = Tree.Node [
        Tree.Leaf "#node: ";
        Tree.Node [
            StrTree.of_int(H2Table.length udag.unique)
        ]
    ]
    
    let dump =
        let dump_leaf = match Header.dump_leaf with
            | Some dump -> dump
            | None        -> (fun _ -> StrTree.of_unit())
        and dump_edge = match Header.dump_edge with
            | Some dump -> dump
            | None        -> (fun _ -> StrTree.of_unit())
        and dump_node = match Header.dump_node with
            | Some dump -> dump
            | None        -> (fun _ -> StrTree.of_unit())
        in
        let dump_next_t (parcours: ident -> unit) = function
            | Utils.Leaf leaf -> [Tree.Leaf "Leaf"; dump_leaf leaf]
            | Utils.Node ident -> parcours ident; [Tree.Leaf "Node"; StrTree.of_int ident]
        in
        let dump_edge_t parcours =
            let dump_next_t = dump_next_t parcours in
            function ((edge, next) : edge_t) ->
                Tree.Node ((dump_edge edge)::(dump_next_t next))
        in
        let dump_node_t parcours =
            let dump_edge_t = dump_edge_t parcours in
            function ((node, edges) : node_t) -> (dump_node node)::(List.map dump_edge_t edges)
        in fun udag edges ->
            let memo = MemoTable.create (H2Table.length udag.unique) in
            let apply : (ident -> unit) -> ident -> unit = MemoTable.apply memo in
            let liste = ref [] in
            let push x =
                liste:=(x::(!liste)) in
            let revret () =
                let temp = List.rev (!liste) in
                liste := [];
                temp
            in

            let rec parcours ident : unit = apply    ((fun ident ->
                (
                    let node : node_t = H2Table.pull udag.unique ident in
                    push (Tree.Node ((StrTree.of_int ident)::(dump_node_t (parcours: ident -> unit) node)));
                )
                                            ):ident -> unit) (ident:ident)
            in
            let liste_edges =    (
                List.map (dump_edge_t parcours) edges
                                )
            in
            let liste_nodes =    revret() in
            Tree.Node [Tree.Node liste_nodes; Tree.Node liste_edges]
    
    let to_dot =
        let dump_leaf = match Header.dot_of_leaf with
            | Some dump -> dump
            | None        -> (fun _ -> "")
        and dump_edge = match Header.dot_of_edge with
            | Some dump -> dump
            | None        -> (fun _ -> "")
        and dump_node = match Header.dot_of_node with
            | Some dump -> dump
            | None        -> (fun _  _ -> "")
        in
        let dump_next_t (parcours: ident -> unit) getleaf = function
            | Utils.Leaf leaf -> "L"^(string_of_int (getleaf leaf))
            | Utils.Node ident -> parcours ident; "N"^(string_of_int ident)
        in
        let dump_edge_t parcours getleaf =
            let dump_next_t = dump_next_t parcours getleaf in
            fun (edge, next) -> (dump_edge edge, dump_next_t next)
        in
        let dump_node_t parcours getleaf =
            let dump_edge_t = dump_edge_t parcours getleaf in
            fun ident (node, edges) -> (dump_node ident node, List.map dump_edge_t edges)
        in fun udag edges print ->
            let memo = MemoTable.create (H2Table.length udag.unique) in
            let memo_leaf = H2Table.create 1 0 in
            let getleaf = H2Table.push memo_leaf in
            let apply : (ident -> unit) -> ident -> unit = MemoTable.apply memo in
            let rec parcours ident : unit = apply    ((fun ident ->
                (
                    let node : node_t = H2Table.pull udag.unique ident in
                    let node_ , edges_ = dump_node_t parcours getleaf ident node in
                    let src_ = "N"^(string_of_int ident) in
                    print ( "\t"^src_ ^ " " ^ node_ ^ "\n");
                    List.iter (fun (edge_, dst_) -> print("\t" ^ src_ ^ " -> " ^ dst_ ^ " " ^ edge_^"\n" )) edges_ ;
                )
                                            ):ident -> unit) (ident:ident)
            in
            H2Table.iter memo_leaf (fun leaf ident ->
				print_string("\tL"^(string_of_int ident)^" "^(dump_leaf leaf));
				print("\tL"^(string_of_int ident)^" "^(dump_leaf leaf))
			);
            List.iteri (fun idx edge ->
                let edge_, dst_ = dump_edge_t parcours getleaf edge in
                print ("\tE"^(string_of_int idx)^" -> "^dst_^" "^edge_^"\n" )) edges
    
    let to_dot_file udag edges target =
        let file = open_out target in
        let print = output_string file in
        print "digraph G {\n\tedge[fontname=\"courier\"];\n";
        to_dot udag edges print;
        print "}\n";
        close_out file

    let load =
        match Header.load_leaf with
            | None -> None
            | Some load_leaf ->
        match Header.load_edge with
            | None -> None
            | Some load_edge ->
        match Header.load_node with
            | None -> None
            | Some load_node ->
        let load_next getid = function
            | [Tree.Leaf "Leaf"; leaf] -> Utils.Leaf (load_leaf leaf)
            | [Tree.Leaf "Node"; ident] ->
                    Utils.Node (getid (StrTree.to_int ident))
            | _ -> assert false
        in
        let load_edge getid = function
            | Tree.Node (edge::noderef) -> (load_edge edge, load_next getid noderef)
            | _ -> assert false
        in
        let load_node getid = function
            | node::edges -> (load_node node, (List.map (load_edge getid) edges))
            | _ -> assert false
        in Some (function
            | Tree.Node [Tree.Node liste_nodes; Tree.Node liste_edges] ->
            (
                let n = List.length liste_nodes in
                let htbl = Hashtbl.create n in
                let getid = Hashtbl.find htbl
                and add = Hashtbl.add htbl
                and mem = Hashtbl.mem htbl in
                let udag = makeman n in
                let load_node = function
                    | Tree.Node (ident::node) ->
                        let ident = StrTree.to_int ident in
                        assert(not(mem ident));
                        add ident ((load_node getid node)|>(push udag))
                    | _ -> assert false
                in
                List.iter load_node liste_nodes;
                let liste_edges = List.map (load_edge getid) liste_edges in
                udag, liste_edges
            )
            | _ -> assert false
                )
    
    module type MODELE_VISITOR =
    sig
        type xedge
        type xnode
        type extra

        val do_leaf : extra -> leaf -> xnode
        val do_edge : extra -> edge -> xnode -> xedge
        val do_node : extra -> node -> xedge list -> xnode
    end

    module VISITOR(M0:MODELE_VISITOR) =
    struct
        type mymanager = {
            man : manager;
            extra : M0.extra;
            calc : edge_t -> M0.xedge;
            memLeaf : (leaf, M0.xnode) MemoTable.t;
            memEdge : ((edge * M0.xnode), M0.xedge) MemoTable.t;
            memNode : (ident, M0.xnode) MemoTable.t;
        }

        type manager = mymanager

        let makeman man extra hsize =
            let memLeaf, memEdge, memNode = MemoTable.(create hsize, create hsize, create hsize) in
            let appLeaf, appEdge, appNode = MemoTable.(apply memLeaf, apply memEdge, apply memNode) in
            let rec calcrec (edge, next) =
                 calcedge edge (match next with
                    | Utils.Leaf leaf -> calcleaf leaf
                    | Utils.Node node -> calcnode node)
            and        calcleaf leaf = appLeaf (M0.do_leaf extra) leaf
            and        calcedge edge xnode = appEdge (fun (edge, xnode) -> M0.do_edge extra edge xnode) (edge, xnode)
            and        calcnode ident = appNode (fun ident ->
                let node, edgelist = pull man ident in
                M0.do_node extra node (List.map calcrec edgelist)) ident
            in
            {
                man = man;
                extra = extra;
                calc = calcrec;
                memLeaf = memLeaf;
                memEdge = memEdge;
                memNode = memNode;
            }, calcrec

        let newman man extra = makeman man extra (H2Table.length man.unique)

        let calc man = man.calc

        let dump_stats man = Tree.Node [
            Tree.Node [Tree.Leaf "memo leaf:"; MemoTable.dump_stats man.memLeaf];
            Tree.Node [Tree.Leaf "memo edge:"; MemoTable.dump_stats man.memEdge];
            Tree.Node [Tree.Leaf "memo node:"; MemoTable.dump_stats man.memNode];
        ]



    end

end


module STRTREE_HEADER : UDAG_HEADER with
        type leaf = StrTree.tree
    and type edge = StrTree.tree
    and type node = StrTree.tree
=
struct
    type leaf = StrTree.tree
    type edge = StrTree.tree
    type node = StrTree.tree

    let dump_leaf = Some (fun x -> x)
    let load_leaf = Some (fun x -> x)
    let dot_of_leaf = Some (fun x -> StrTree.dump_tree x)

    let dump_edge = Some (fun x -> x)
    let load_edge = Some (fun x -> x)
    let dot_of_edge = Some (fun x -> StrTree.dump_tree x)

    let dump_node = Some (fun x -> x)
    let load_node = Some (fun x -> x)
    let dot_of_node = Some (fun _ x -> StrTree.dump_tree x)
end


module STRING_HEADER : UDAG_HEADER with
        type leaf = string
    and type edge = string
    and type node = (int option) * string
=
struct
    type leaf = string
    type edge = string
    type node = (int option) * string

    let dump_leaf = Some (fun x -> Tree.Leaf x)
    let load_leaf = Some (function Tree.Leaf x -> x | _ -> assert false)
    let dot_of_leaf = Some (fun x -> x)

    let dump_edge = Some (fun x -> Tree.Leaf x)
    let load_edge = Some (function Tree.Leaf x -> x | _ -> assert false)
    let dot_of_edge = Some (fun x -> x)

    let dump_node = Some (function (None, x) -> Tree.Leaf x | (Some i, x) -> Tree.Node [StrTree.of_int i; Tree.Leaf x])
    let load_node = Some (function
        | Tree.Leaf x -> (None, x)
        | Tree.Node [i; Tree.Leaf x] -> (Some(StrTree.to_int i), x)
        | _ -> assert false)
    let dot_of_node = Some (fun _ (_, x) -> x)
end



module String =
struct
    include UDAG(STRING_HEADER)
    let load = match load with
        | Some f    -> f
        | None        -> assert false
    
    let loadfile target =
        load (match StrTree.loadfile target with [] -> assert false | x::_ -> x)
    
    let dumpfile udag edges target =
        StrTree.dumpfile [dump udag edges] target

end

module StrTree =
struct
    include UDAG(STRTREE_HEADER)
    let load = match load with
        | Some f    -> f
        | None        -> assert false
    
    let loadfile target =
        load (match StrTree.loadfile target with [] -> assert false | x::_ -> x)
    
    let dumpfile udag edges target =
        StrTree.dumpfile [dump udag edges] target
end
