module type MODULE_UBDAG =
sig
    module H:Udag.UDAG_HEADER

    type ident = int
(*    type ident *)

    type tree  = (H.leaf, pnode) Utils.gnode
    and  node  = H.node * tree * tree
    and  pnode = ident
(*    and     pnode *)

    val get_ident : pnode -> ident
    
    val equal_tree : tree -> tree -> bool
    val equal_node : node -> node -> bool

    type manager

    val makeman : int -> manager

    val newman : unit -> manager
    val push : manager -> node -> pnode
    val pull : manager -> pnode -> node

    val dump_stats : manager -> StrTree.tree

end


module UBDAG(H0:Udag.UDAG_HEADER) : MODULE_UBDAG with
        type H.node = H0.node
    and type H.leaf = H0.leaf
=
struct
    type ident = int

    module H = H0
    type tree = (H.leaf, pnode) Utils.gnode
    and     node  = (H.node * tree * tree)
    and  pnode = ident

    let get_ident pnode = pnode
    
    let equal_tree x y = match x, y with
        | Utils.Leaf x, Utils.Leaf y -> x = y
        | Utils.Node x, Utils.Node y -> (x = y)
        | _ -> false

    let equal_node (d, t0, t1) (d', t0', t1') =
        (d = d')&&(equal_tree t0 t0')&&(equal_tree t1 t1')

    (* let dump man ed *)

    type manager = {
        unique : node H2Table.t;
    }

    let makeman hsize = {
        unique = H2Table.create hsize 0;
    }

    let default_newman_hsize = 10000

    let newman () = makeman default_newman_hsize
    let push man node = H2Table.push man.unique node
    
    let pull man pnode = H2Table.pull man.unique pnode

    let dump_stats man = Tree.Node [
        Tree.Leaf "#node: "; Tree.Node [ StrTree.of_int (H2Table.length man.unique) ]
    ]

end

