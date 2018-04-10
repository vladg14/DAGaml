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

	type uint =
	{
		arity : int;				(* arity of each [edge], usually arity <> Array.length array *)
		array : edge array
	}

	let uint_get ?default x intX = if x < (Array.length intX.array)
		then intX.array.(x)
		else (match default with Some default -> default | None -> (cst false intX.arity))

	let map f intX = {arity = intX.arity; array = Array.map f intX.array}

	let uint_of_bool edge = {arity = arity edge; array = [|edge|]}

	let uint_of_int arity x =
		assert(x>=0);
		let bin = Tools.bin_of_int x in
		let array = Array.map (fun b -> cst b arity) bin in
		{arity; array}

	let uint_one arity = {arity; array = [|cst true arity|]}

	let add ?carry intX intY =
		let lenX = Array.length intX.array
		and lenY = Array.length intY.array in
		assert(intY.arity = intX.arity);
		let carry = ref (match carry with
			| None -> (cst false intX.arity)
			| Some edge -> assert(arity edge = intX.arity); edge)
		in
		let arity = intX.arity in
		let len = max lenX lenY in
		{
			arity;
			array = Array.init (len+1) (fun i ->
				if i = len
				then (!carry)
				else
				(
					let c = !carry in
					let x = uint_get i intX
					and y = uint_get i intY in
					let xy, c = fulladd3 x y c in
					carry := c;
					xy
				)
			)
		}

	let ( +/ ) x y = add x y

	let array_add array = Tools.tree_of_array add array
	let  list_add liste = Tools.tree_of_list  add liste

	let ( +?/ ) uintX intY = uintX +/ (uint_of_int uintX.arity intY)
	let ( ?+/ ) intX uintY = (uint_of_int uintY.arity intX) +/ uintY

	(* decreasing shift *)
	let shift_right shift intX =
		let len = Array.length intX.array in
		if shift < len
		then
		(
			let array = Array.init (len-shift) (fun i -> intX.array.(i+shift)) in
			{arity = intX.arity; array}
		)
		else {arity = intX.arity; array = [||]}

	let ( >>/ ) intX shift = shift_right shift intX

	(* increasing shift *)
	let shift_left shift intX =
		let len = Array.length intX.array
		and zero = cst false intX.arity in
		let array = Array.init (len+shift) (fun i ->
			let i' = i - shift in
			if i' >= 0 then intX.array.(i') else zero) in
		{arity = intX.arity; array}

	let ( <</ ) intX shift = shift_left shift intX

	let bitwise_binop binop ?defaultX ?defaultY intX intY =
		let ari = intX.arity
		and lenX = Array.length intX.array
		and lenY = Array.length intY.array in
		let defaultX = match defaultX with Some edge -> edge | None -> (cst false intX.arity)
		and defaultY = match defaultY with Some edge -> edge | None -> (cst false intY.arity) in
		assert(ari = intY.arity);
		{
			arity = ari;
			array = Array.init (max lenX lenY) (fun i ->
				let x = uint_get ~default:defaultX i intX
				and y = uint_get ~default:defaultY i intY in
				binop x y)
		}

	let ( |&/ ) intX intY = bitwise_binop ( &! ) intX intY
	let ( |^/ ) intX intY = bitwise_binop ( ^! ) intX intY
	let ( ||/ ) intX intY = bitwise_binop ( |! ) intX intY
	let ( |=/ ) intX intY = bitwise_binop ( =! ) intX intY
	let ( |*/ ) intX intY = bitwise_binop ( *! ) intX intY
	
	let ( =/ ) intX intY = G0.array_and (intX |=/ intY).array
	let ( <>/ ) intX intY = neg(intX =/ intY)

	let bitwise_choice bX int0 int1 =
		bitwise_binop (G0.ite bX) int0 int1

	let zero_choice bX intY =
		bitwise_choice bX {arity = intY.arity; array = [||]} intY

	let shift_right' shiftX intY =
		let result = ref intY in
		for i = 0 to (Array.length shiftX.array - 1)
		do
			result := bitwise_choice shiftX.array.(i) !result (shift_right (1 lsl i) !result);
		done;
		!result

	let ( >>// ) intY shiftX = shift_right' shiftX intY

	let shift_left' shiftX intY =
		let result = ref intY in
		for i = 0 to (Array.length shiftX.array - 1)
		do
			result := bitwise_choice shiftX.array.(i) !result (shift_left (1 lsl i) !result);
		done;
		!result

	let ( <<// ) intY shiftX = shift_left' shiftX intY

	let card bXarray = array_add (Array.map uint_of_bool bXarray)

	let exp_card arity bXarray =
		let result = ref (uint_one arity) in
		Array.iter (fun bX ->
			assert(M0.arity bX = arity);
			result := bitwise_choice bX !result (shift_left 1 !result);
		) bXarray;
		!result

	let exp_card' arity bXarray =
		(uint_one arity) >>// (card bXarray)

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

	let lX (default:bool) (intX:uint) (intY:uint) =
		let ( <! ) x y = (neg x) &! y in
		let arity = intX.arity in
		assert(arity = intY.arity);
		let length = max (Array.length intX.array) (Array.length intY.array) in
		let carry = ref (cst default arity) in
		for i = 0 to length - 1
		do
			let x = uint_get i intX
			and y = uint_get i intY in
			carry := G0.ite (x =! y) (x <! y) !carry;
		done;
		!carry

	let lt = lX false
	let ( </ ) = lt

	let le = lX true
	let ( <=/ ) = le

	let gX default intX intY = lX default intY intX

	let gt = gX false
	let ( >/ ) = gt

	let ge = gX true
	let ( >=/ ) = ge

	let copy ncopy myuint =
		let mat = Matrix.array2_of_matrix (G0.array_copy_fun_array   ncopy myuint.array) in
		Array.map (fun array -> { arity = ncopy * myuint.arity; array}) mat

	let copy_t ncopy myuint =
		let mat = Matrix.array2_of_matrix (G0.array_copy_fun_array_t ncopy myuint.array) in
		Array.map (fun array -> {arity = ncopy * myuint.arity; array}) mat

	let input arity = {arity; array = G0.array_make_n_var arity}

	let range n =
		let bin = Tools.bin_of_int (n-1) in
		let arity = Array.length bin in
		let array = Array.map (fun b -> cst b arity) bin in
		let myuint = input arity in
		myuint, (myuint <=/ {arity; array})
end

(*
	let multiply intX intY =
		let ari = intX.arity
		and len = Array.length intX.array in
		assert(ari = intY.arity);
		assert(len = Array.length intY.array);
*)	

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
