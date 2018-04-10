type args =
| Stats of (Tree.stree -> unit)

let get_stats liste = MyList.opmap (function Stats func -> Some func | _ -> None) liste
let map_stats stats args = match get_stats args with
	| [] -> ()
	| liste ->
	(
		let stats = stats() in
		List.iter (fun func -> func stats) liste
	)
let print_stats = Stats(fun stree -> STree.pprint [stree]; print_newline())
