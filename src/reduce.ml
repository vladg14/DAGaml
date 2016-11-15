open Extra
open Udag

module Create(Src:GRAPH)(Dst:GRAPH)
=
struct
    type next_t =
        | Leaf of Dst.leaf
        | NodeRef of Dst.ident
        | Node of node_t
    and edge_t = Dst.edge * next_t
    and node_t = Dst.node * (edge_t list)

    type manager = {
        memotable : (Src.ident, Dst.edge_t) MemoTable.t;
    }

    let newman hsize = {
        memotable = MemoTable.create hsize;
    }

    let push dst_udag =
        let rec pnext = function
            | Leaf leaf     -> Dst.Leaf leaf
            | NodeRef ident -> Dst.NodeRef ident
            | Node node     -> Dst.NodeRef (pnode node)
        and     pedge = function ((edge, next):edge_t) ->
            (edge, pnext next)
        and     pnode = function ((node, edges):node_t) ->
            Dst.push dst_udag (node, List.map pedge edges)
        in pedge

    let apply man src_udag dst_udag fleaf fedge fnode edges =
        let apply = MemoTable.apply man.memotable in
        let pnext p = function
            | Src.Leaf leaf -> fleaf leaf
            | Src.NodeRef ident -> p ident
        in
        let pedge p = function (edge, next) ->
            fedge edge (pnext p next)
        in
        let pnode p = function (node, edges) ->
            fnode node (List.map (pedge p) edges)
        in
        let pull = Src.pull src_udag
        in
        let rec pident (ident : Src.ident) =
            apply
                (pull >> (pnode pident) >> (push dst_udag))
                ident
        in
        List.map (pedge pident) edges

end

        
