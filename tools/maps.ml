let map_power items size func =
	let rec aux start carry = function
		| 0 -> (func start)::carry
		| n -> List.fold_left (fun carry item -> aux (item::start) carry (n-1)) carry items
	in aux [] [] size

let map_power_cat items size func = 
	List.concat (map_power items size func)

let map_halfsquare_power items sizeX sizeY filter func =
	let rec aux start sizeX = function
		| 0 -> if not(filter start) then [] else [func start]
		| sizeY	->
			if not(filter start) then [] else	(
			List.concat (map_power items sizeX (fun item -> aux (item::start) (sizeX-1) (sizeY-1)))
												)
	in aux [] sizeX sizeY

let rec range = function
	| n when n < 0 -> assert false
	| 0 -> [0]
	| n -> n::(range (n-1))
;;

let ordpower top size func =
	let rec aux start carry top = function
		| 0	-> (func start)::carry
		| n -> List.fold_left (fun carry item -> aux (item::start) carry item (n-1)) carry (range top)
	in aux [] [] top size
