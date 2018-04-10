module MODULE(M0:GOops.MODELE) =
struct
	open M0
	module G0 = GOops.MODULE(M0)

	let fulladd2 a b =
		let carry = a |! b in
		let somme = a ^! b in
		(somme, carry)

	let fulladd3 a b c = 
		let carry =  (a |! (b &! c)) &! (b |! c) in
		let somme = a ^! b ^! c in
		(somme, carry)

	type sized_uint = {
		arity : int;
		array : edge array;
	}

	let map f intX = {arity = intX.arity; array = Array.map f intX.array}

	let add ?carry intX intY =
		let ari = intX.arity
		and len = Array.length intX.array in
		assert(ari = intY.arity);
		assert(len = Array.length intY.array);
		let carry = ref (match carry with
			| None -> (cst false ari)
			| Some edge -> assert(arity edge = ari); edge)
		in
		let intXY = {
			arity = ari;
			array = Array.init len (fun i ->
				let x = intX.array.(i)
				and y = intY.array.(i)
				and c = !carry in
				let xy, c = fulladd3 x y c in
				carry := c;
				xy)
		} in
		(intXY, !carry)

	let ( +/ ) x y = add x y |> fst

	(* zero padding shift left *)
	let shift_right shift intX =
		let len = Array.length intX.array in
		assert(shift <= len);
		let zero = cst false intX.arity in
		let array = Array.init len (fun i ->
			let i' = i + shift in
			if i' < len then intX.array.(i') else zero) in
		{arity = intX.arity; array}

	let ( >>/ ) intX shift = shift_right shift intX

	let shift_left shift intX =
		let len = Array.length intX.array in
		assert(shift <= len);
		let zero = cst false intX.arity in
		let array = Array.init len (fun i ->
			let i' = i - shift in
			if i' >= 0 then intX.array.(i') else zero) in
		{arity = intX.arity; array}

	let ( <</ ) intX shift = shift_left shift intX

	let bitwise_binop binop intX intY =
		let ari = intX.arity
		and len = Array.length intX.array in
		assert(ari = intY.arity);
		assert(len = Array.length intY.array);
		{arity = ari; array = Array.map2 binop intX.array intY.array}

	let ( |&/ ) = bitwise_binop ( &! )
	let ( |^/ ) = bitwise_binop ( ^! )
	let ( ||/ ) = bitwise_binop ( |! )
	let ( |=/ ) = bitwise_binop ( =! )
	let ( |*/ ) = bitwise_binop ( *! )

	let ( =/ ) intX intY = G0.array_and (intX |=/ intY).array

	let scalar_binop_left binop bitX intY =
		assert(arity bitX = intY.arity);
		map (binop bitX) intY

	let scalar_binop_right binop intX bitY =
		assert(arity bitY = intX.arity);
		map (fun bit -> binop bit bitY) intX

	let ( |.&/ ) = scalar_binop_left ( &! )
	let ( |.^/ ) = scalar_binop_left ( ^! )
	let ( |.|/ ) = scalar_binop_left ( |! )
	let ( |.=/ ) = scalar_binop_left ( =! )
	let ( |.*/ ) = scalar_binop_left ( *! )

	let ( |&./ ) = scalar_binop_right ( &! )
	let ( |^./ ) = scalar_binop_right ( ^! )
	let ( ||./ ) = scalar_binop_right ( |! )
	let ( |=./ ) = scalar_binop_right ( =! )
	let ( |*./ ) = scalar_binop_right ( *! )

(*
	let multiply intX intY =
		let ari = intX.arity
		and len = Array.length intX.array in
		assert(ari = intY.arity);
		assert(len = Array.length intY.array);
*)	

end

(*
let int_mult nvars x y =
	let x, y = if Array.length x <= Array.length y then x, y else y, x in
	let n = Array.length x in
	if Array.length x = 0
	then {nvars = nvars; shift = 0; array = [| |]}
	else
	(
		let funmap i xi = {nvars = nvars; shift = i; array = int_scalarmult xi y} in
		let square  = Array.mapi funmap x in
		let rec aux s i =
			print_string "x:"; print_int i; print_string "\r"; flush stdout;
			if i = n
			then s
			else aux (shiftint_fulladd s square.(i)) (i+1)
		in
		aux (square.(0)) 1
	)
*)
