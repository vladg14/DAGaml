type 'a ostream = {
	os_elem : 'a -> unit;
	os_list : 'a list -> unit;
	os_iter : 'a Iter.iter -> unit;
	os_stream : 'a Stream.t -> unit;
	mutable locked : bool;
	closure : unit -> unit;
}

let lock os =
	assert(not os.locked);
	os.locked <- true;
	(fun () -> os.locked <- false; ())

let pipe_list (conv : 'a -> 'b list) (os : 'b ostream) : 'a ostream =
	let closure = lock os in
	let os_elem x = os.os_list (conv x) in
	let os_list l = os.os_list (MyList.catmap conv l) in
	let os_iter i = os.os_iter Extra.(IterExtra.( i $@ (conv >> Iter.of_list) )) in
	let os_stream stream =
		let buffer = ref [] in
		let rec load () = match Stream.peek stream with
				| None -> None
				| Some a ->
				(
					Stream.junk stream;
					match conv a with
					| [] -> load ()
					| [b] -> Some b
					| b::t -> buffer := t; Some b
				)
		in
		os.os_stream (Stream.from (fun _ -> match !buffer with
			| [] -> load ()
			| h::t -> buffer := t; Some h))
	in
	{os_elem; os_list; os_iter; os_stream; locked = false; closure}

let pipe (conv : 'a -> 'b) (os : 'b ostream) : 'a ostream =
	let closure = lock os in
	let os_elem x = os.os_elem (conv x) in
	let os_list l = os.os_list (List.map conv l) in
	let os_iter i = os.os_iter IterExtra.( i $$ conv ) in
	let os_stream stream = os.os_stream (Stream.from (fun _ -> match Stream.peek stream with None -> None | Some a -> Stream.junk stream; Some(conv a))) in
	{os_elem; os_list; os_iter; os_stream; locked = false; closure}

let pipe_iter (conv : 'a -> 'b Iter.iter) (os : 'b ostream) : 'a ostream =
	(* TODO : we should be able to select the closure policy *)
	let closure = lock os in
	let os_elem x = os.os_iter (conv x) in
	let os_list l = os.os_iter IterExtra.( Iter.of_list l $@ conv ) in
	let os_iter i = os.os_iter IterExtra.( i $@ conv ) in
	let os_stream stream =
		let iter = ref (Iter.stop) in
		let rec load () = match Stream.peek stream with
			| None -> None
			| Some a ->
			(
				Stream.junk stream;
				match Iter.pull (conv a) with
				| None -> load ()
				| Some (h, t) -> iter := t; Some h
			)
		in
		os.os_stream (Stream.from (fun _ -> match Iter.pull (!iter) with
			| None -> load ()
			| Some (h, t) -> iter := t; Some h))
	in
	{os_elem; os_list; os_iter; os_stream; locked = false; closure}

let output_elem os elem = assert(not os.locked); os.os_elem elem
let output_list os list = assert(not os.locked); os.os_list list
let output_iter os iter = assert(not os.locked); os.os_list iter
let output_stream os stream = assert(not os.locked); os.os_list stream

let from_output_elem closure output_elem : _ ostream = {
	os_elem = output_elem;
	os_list = List.iter output_elem;
	os_iter = Iter.iter output_elem;
	os_stream = Stream.iter output_elem;
	locked = false;
	closure;
}

let from_channel out_channel =
	from_output_elem (fun () -> close_out out_channel) (output_char out_channel)

let bool_ostream_of_char_ostream os =
	assert(not os.locked);
	os.locked <- false;
	let buffer = ref [] in
	let os_elem x =
		buffer := x::(!buffer);
		if List.length !buffer = 8
		then
		(
			os.os_elem (Tools.char_of_bin (List.rev !buffer));
			buffer := [];
		);
		assert(List.length !buffer < 8)
	in
	let closure () =
		os.os_elem (Tools.char_of_bin (List.rev !buffer));
		os.closure ()
	in
	from_output_elem closure os_elem
	
