type graph = {
(* List of valid nodes *)
	nodes : int list;
(* adjacence matrix *)
	edges : bool Matrix.matrix;
}

let la_of_aa aa =
	let nodes = aa.nodes in
	let funmap a = a |> Array.to_list |> MyList.indexes (fun x -> x) in
	let edges = Array.map funmap aa.edges.Matrix.a in
	GraphLA.{nodes; edges}

(* let la_of_aa aa *)

let make n b = {nodes = MyList.init n (fun i -> i); edges = Matrix.make (n, n) b}

let set graph edge = Matrix.set graph.edges edge true
let get graph edge = Matrix.get graph.edges edge

let closure graph = {
	nodes = graph.nodes;
	edges = Matrix.expmat  ( || ) ( && ) false true graph.edges (List.length graph.nodes);
}

type 'a o3graph = {
	dump : 'a -> int;
	load : int -> 'a;
	graph : graph;
}

let o3make dump load n b = {dump; load; graph = make n b}

let o3set graph (x, y) = set graph.graph (graph.dump x, graph.dump y)
let o3get graph (x, y) = get graph.graph (graph.dump x, graph.dump y)

let o3get_bidir graph (x, y) = (o3get graph (x, y)) && (o3get graph (y, x))

let o3closure graph = {
	dump = graph.dump;
	load = graph.load;
	graph = closure graph.graph;
}
