
let of_char_stream stream =
	let buffer = ref [] in
	Stream.from (fun _ -> match !buffer with
		| [] ->
		(
			match Stream.peek stream with
			| None -> None
			| Some c -> Stream.junk stream; (match Tools.bin_of_char c with
				| [] -> assert false
				| h::t -> buffer := t; Some h)
		)
		| h::t -> buffer := t; Some h
	)

let to_char_stream stream =
	Stream.from (fun _ -> match Stream.npeek 8 stream with
		| [] -> None
		| bl -> Some(Tools.char_of_bin bl))

type out_channel = {
	mutable out_buffer : bool list;
	out_channel : Pervasives.out_channel;
}

type in_channel = {
	mutable in_buffer : bool list;
	in_channel : Pervasives.in_channel;
}

let open_out target = {
	out_buffer = [];
	out_channel = Pervasives.open_out_bin target;
}

let open_in target = {
	in_buffer = [];
	in_channel = Pervasives.open_in_bin target;
}

let output_bool_list out_channel bl : unit =
	let buffer = out_channel.out_buffer @ bl in
	let rec aux = function
		| x0::x1::x2::x3::x4::x5::x6::x7::buffer ->
		(
			output_char out_channel.out_channel (Tools.char_of_bin [x0; x1; x2; x3; x4; x5; x6; x7]);
			aux buffer
		)
		| buffer -> buffer
	in
	out_channel.out_buffer <- (aux buffer)

let output_bool out_channel b = output_bool_list out_channel [b]

let input_bool_list in_channel n : bool list =
	let n' = n - List.length in_channel.in_buffer in
	let k = (n'+7)/8 in (* up rounded division by 8 *)
	let rec aux carry = function
		| 0 -> List.rev carry
		| n -> aux ((input_char in_channel.in_channel |> Tools.bin_of_char )::carry) (n-1)
	in
	let buffer = List.concat (in_channel.in_buffer::(aux [] k)) in
	let bl, buffer = MyList.hdtl_nth n buffer in
	in_channel.in_buffer <- buffer;
	bl

let input_bool in_channel : bool = match input_bool_list in_channel 1 with [x] -> x | _  -> assert false


let close_in in_channel = close_in in_channel.in_channel

let close_out out_channel =
	match out_channel.out_buffer with
	| [] -> ()
	| buffer -> output_char out_channel.out_channel (Tools.char_of_bin buffer);
	close_out out_channel.out_channel



let flush out_channel = flush out_channel.out_channel

let output_unit (oc : out_channel) () = ()
let input_unit (ic : in_channel) = ()

let output_option oc output_some = function
	| None -> output_bool oc false
	| Some x -> output_bool oc true; output_some oc x

let input_option ic input_some = if input_bool ic 
	then (Some(input_some ic))
	else None

let output_sized_list oc output_elem : _ list -> unit = List.iter (output_elem oc)
let output_none_list oc output_option liste : unit = List.iter (fun x -> output_option oc (Some x)) liste; output_option oc None


let output_list oc output_elem liste : unit = output_none_list oc (fun oc -> output_option oc output_elem) liste

let input_sized_list ic input_elem n : _ list =
	let rec aux carry = function
		| 0 -> List.rev carry
		| n -> aux ((input_elem ic)::carry) (n-1)
	in aux [] n

let input_none_list ic input_option : _ list =
	let rec aux carry = match input_option ic with
		| None -> List.rev carry
		| Some x -> aux (x::carry)
	in aux []

let input_list ic input_elem : _ list = input_none_list ic (fun ic -> input_option ic input_elem)

let output_sized_array oc output_elem array = output_sized_list oc output_elem (Array.to_list array)
let input_sized_array ic input_elem n = Array.of_list (input_sized_list ic input_elem n)

let output_int_pos oc n =
	assert(n>=0);
	let rec aux c0 c1 = function
		| 0 -> c0@[true]@c1
		| n -> aux (false::c0) ((n mod 2 = 1)::c1) (n/2)
	in
	output_bool_list oc (aux [] [] n);
	()

let input_unary ic =
	let rec aux carry = if input_bool ic
		then carry
		else (aux (carry+1))
	in aux 0

let input_int_pos ic =
	Tools.int_of_bin (List.rev(input_bool_list ic (input_unary ic)))

let output_array oc output_elem array =
	output_int_pos oc (Array.length array);
	output_sized_array oc output_elem array;
	()

let input_array ic input_elem array =
	let n = input_int_pos ic in
	input_sized_array ic input_elem n

let output_bitv oc bitv = output_bool_list oc (Bitv.L.to_bool_list bitv)
let input_bitv ic size = Bitv.L.of_bool_list (input_bool_list ic size)
