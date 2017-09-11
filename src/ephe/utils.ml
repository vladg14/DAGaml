type ('edge, 'node) merge =
	| MEdge of 'edge
	| MNode of 'node

type ('return, 'node) pull_request = ('return, 'node -> 'return) merge

type ('edge, 'node) unmerge = ('edge * 'edge, 'node) pull_request

type ('tag, 'edge, 'node) unmerge_tagged = ('tag * 'edge * 'edge, 'node) pull_request

type ('leaf, 'node) gnode =
	| Leaf of 'leaf
	| Node of 'node
