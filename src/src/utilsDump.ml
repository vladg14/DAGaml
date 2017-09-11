open Utils
open StrDump

let gnode leaf node = function
	| Leaf leaf' -> "Leaf ("^(leaf leaf')^")"
	| Node node' -> "Node ("^(node node')^")"

let merge next node = function
	| MEdge next' -> "MEdge ("^(next next')^")"
	| MNode node' -> "MNode ("^(node node')^")"
let emerge block merge emerge' = pair block merge emerge'

let merge3 next node = function
	| M3Edge edge' -> "M3Edge ("^(next edge')^")"
	| M3Cons node' -> "M3Cons ("^(node node')^")"
	| M3Node node' -> "M3Node ("^(node node')^")"
let emerge3 block merge3 emerge3' = pair block merge3 emerge3'
