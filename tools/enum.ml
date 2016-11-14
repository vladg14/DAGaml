let make_comp_uncomp liste =
	let n = List.length liste in
	let k = Tools.math_log 2 n in
	let mask = BigInt.(-) (BigInt.shift_left BigInt.unit k) BigInt.unit in
	let comp' items =
		let data = List.fold_left	(fun acc item ->
			let index = match MyList.list_index item liste with
				| None -> assert false
				| Some index -> index
			in
			BigInt.(||) (BigInt.shift_left acc k) (BigInt.of_int index)
						) BigInt.zero items
		in
		(List.length items, data)
	and uncomp' (len, data) =
		assert(len>=0);
		let rec aux carry data = function
			| 0		-> carry
			| len	->	(
				let index = BigInt.to_int (BigInt.(&&) mask data)in
				assert(index<n);
				aux ((List.nth liste index)::carry) (BigInt.shift_right_towards_zero data k) (len-1)
						)
		in aux [] data len
	in
	let comp items =
		let comped = comp' items in
		assert(uncomp' comped = items);
		comped
	and uncomp (len, data) =
		let uncomped = uncomp' (len, data) in
		assert(comp' uncomped = (len, data));
		uncomped
	in
	comp, uncomp

let hard_comp_uncomp liste =
	let n = List.length liste in
	let big_n = BigInt.of_int n in
	let comp' items =
		let data = List.fold_left
			(fun acc item ->
				let index = match MyList.list_index item liste with
					| None -> assert false
					| Some index -> index
				in BigInt.( + ) (BigInt.( * ) acc big_n) (BigInt.of_int index)
			) BigInt.zero items
		in (List.length items, data)
	and uncomp' (len, data) =
		assert(len>=0);
		let rec aux carry data = function
			| 0 -> carry
			| len ->	(
				let index = BigInt.to_int (BigInt.modulo data big_n) in
				aux ((List.nth liste index)::carry) (BigInt.(/) data big_n) (len-1)
						)
		in aux [] data len
	in
	let comp uncomped =
		let comped = comp' uncomped in
		assert(uncomp' comped = uncomped);
		comped
	and uncomp comped =
		let uncomped = uncomp' comped in
		assert(comp' uncomped = comped);
		uncomped
	in
	comp, uncomp
