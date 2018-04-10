exception StrError of string

type ('edge, 'node) merge =
	| MEdge of 'edge
	| MNode of 'node

let dump_merge dump_medge dump_mnode merge stream = match merge with
	| MEdge medge -> false::(dump_medge medge stream)
	| MNode mnode -> true ::(dump_mnode mnode stream)

let load_merge load_medge load_mnode merge = function
	| false::stream ->
	(
		let medge, stream = load_medge stream in
		(MEdge medge, stream)
	)
	| true ::stream ->
	(
		let mnode, stream = load_mnode stream in
		(MNode mnode, stream)
	)
	| _ -> assert false

let o3s_merge (dump_medge, load_medge) (dump_mnode, load_mnode) =
(
	dump_merge dump_medge dump_mnode,
	load_merge load_medge load_mnode
)

type ('return, 'node) pull_request = ('return, 'node -> 'return) merge

type ('edge, 'node) unmerge = ('edge * 'edge, 'node) pull_request

type ('tag, 'edge, 'node) unmerge_tagged = ('tag * 'edge * 'edge, 'node) pull_request

type ('leaf, 'node) gnode =
	| Leaf of 'leaf
	| Node of 'node


let gnode_leaf = function
	| Leaf leaf -> leaf
	| Node _ -> assert false

let gnode_node = function
	| Leaf _ -> assert false
	| Node node -> node

let dump_gnode dump_leaf dump_node gnode stream = match gnode with
	| Leaf leaf -> false::(dump_leaf leaf stream)
	| Node node -> true ::(dump_node node stream)

let load_gnode load_leaf load_node = function
	| false::stream ->
	(
		let leaf, stream = load_leaf stream in
		(Leaf leaf, stream)
	)
	| true ::stream ->
	(
		let node, stream = load_node stream in
		(Node node, stream)
	)
	| _ -> assert false


let o3s_gnode
	((dump_leaf, load_leaf) : 'leaf BinO3.o3s)
	((dump_node, load_node) : 'node BinO3.o3s) : ('leaf, 'node) gnode BinO3.o3s =
(
	dump_gnode dump_leaf dump_node,
	load_gnode load_leaf load_node
)

let kldump_gnode dump_leaf dump_node gnode stream = match gnode with
	| Leaf leaf -> let (k, l) = dump_leaf leaf stream in (false::k, l)
	| Node node -> let (k, l) = dump_node node stream in (true ::k, l)

let klload_gnode load_leaf load_node (k, l) = match k with
	| false::k -> let leaf, stream = load_leaf (k, l) in Leaf leaf
	| true ::k -> let node, stream = load_node (k, l) in Node node
	| [] -> assert false

let klo3s_gnode (dump_leaf, load_leaf) (dump_node, load_node) =
(
	kldump_gnode dump_leaf dump_node,
	klload_gnode load_leaf load_node
)

let gnode_is_leaf = function
	| Leaf _ -> true
	| Node _ -> false

type ('stop, 'left, 'right, 'both) binpull =
	| MStop of 'stop
	| Go0   of 'left
	| Go1   of 'right
	| MPull	of 'both

type ('return, 'edge, 'node) binpath = ('return, 'edge -> 'return, 'edge -> 'return, 'node -> 'return) binpull

type ('edge, 'cons, 'node) merge3 =
	| M3Edge of 'edge
	| M3Cons of 'cons
	| M3Node of 'node

type ('edge, 'node, 'leaf, 'link) node =
	| TNode of ('node * (('edge, 'node, 'leaf, 'link) edge * ('edge, 'node, 'leaf, 'link) edge))
	| TLeaf of 'leaf
	| TLink of 'link
and  ('edge, 'node, 'leaf, 'link) edge = 'edge * ('edge, 'node, 'leaf, 'link) node

type ('pnode, 'tnode) pt_node =
	| PTree of 'pnode
	| TTree of 'tnode


let pnext_of_next = function
	| Leaf leaf -> Leaf leaf
	| Node node -> Node (None, node)

let pedge_of_edge (edge, next) =
	(edge, pnext_of_next next)

let pnode_of_node (node, edge0, edge1) =
	(node, pedge_of_edge edge0, pedge_of_edge edge1)

let merge_of_edge (edge, next) = (edge, MEdge next)
let merge3_of_edge (edge, next) = (edge, M3Edge next)
