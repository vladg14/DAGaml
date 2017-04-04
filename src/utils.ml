type ('edge, 'node) merge =
	| MEdge of 'edge
	| MNode of 'node

type ('return, 'node) pull_request = ('return, 'node -> 'return) merge

type ('edge, 'node) unmerge = ('edge * 'edge, 'node) pull_request

type ('tag, 'edge, 'node) unmerge_tagged = ('tag * 'edge * 'edge, 'node) pull_request

type ('leaf, 'node) gnode =
	| Leaf of 'leaf
	| Node of 'node

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
