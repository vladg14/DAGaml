let init n = Array.init n (fun x -> x)

let rec find array x =
	if array.(x) = x
	then x
	else
	(
		let x' = find array (array.(x)) in
		array.(x) <- x';
		x'
	)

let union array x y =
	let x' = find array x
	and y' = find array y in
	if x' < y'
	then array.(y') <- x'
	else if y' < x'
	then array.(x') <- y'
	else ()

let remap array =
	let remap = Array.make (Array.length array) 0 in
	let rec aux n i = 
		if i = Array.length array
		then ()
		else
		(
			let x = array.(i) in
			if i = x
			then (remap.(i) <- n; aux (n+1) (i+1))
			else (remap.(i) <- remap.(x); aux n (i+1))
		)
	in
	aux 0 0;
	remap

let ends array =
	Array.iteri (fun i _ -> ignore(find array i)) array;
(*	Array.iter (fun x -> print_int x; print_string " ") array;
	print_newline(); *)
	Array.iteri (fun i x -> assert((x=i)||(array.(x)=x))) array;
	let remap = remap array in
	let parts = Array.make (Array.length remap) [] in
	Array.iteri (fun i x -> parts.(x) <- i::(parts.(x))) remap;
	(remap, parts)
