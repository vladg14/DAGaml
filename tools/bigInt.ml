open Big_int

type big_int = Big_int.big_int;;
	
let zero = Big_int.zero_big_int;;
let unit = Big_int.unit_big_int;;
let minus = Big_int.minus_big_int;;
let abs = Big_int.abs_big_int;;
let ( + ) = Big_int.add_big_int;;
let succ = Big_int.succ_big_int;;
let addi = Big_int.add_int_big_int;;
let ( - ) = Big_int.sub_big_int;;
let pred = Big_int.pred_big_int;;
let ( * ) = Big_int.mult_big_int;;
let multi = Big_int.mult_int_big_int;;
let square = Big_int.square_big_int;;
let sqrt = Big_int.sqrt_big_int;;
let quomod = Big_int.quomod_big_int;;
let ( / ) = Big_int.div_big_int;;
let modulo = Big_int.mod_big_int;;
let gcd = Big_int.gcd_big_int;;
let pow_ii = Big_int.power_int_positive_int;;
let pow_bi = Big_int.power_big_int_positive_int;;
let pow_ib = Big_int.power_int_positive_big_int;;
let pow = Big_int.power_big_int_positive_big_int;;
let sign = Big_int.sign_big_int;;
let compare = Big_int.compare_big_int;;
let ( = ) = Big_int.eq_big_int;;
let ( <= ) = Big_int.le_big_int;;
let ( >= ) = Big_int.ge_big_int;;
let ( < ) = Big_int.lt_big_int;;
let ( > ) = Big_int.gt_big_int;;
let max = Big_int.max_big_int;;
let min = Big_int.min_big_int;;
let len = Big_int.num_digits_big_int;;
let to_string = Big_int.string_of_big_int;;
let of_string = Big_int.big_int_of_string;;
let of_int = Big_int.big_int_of_int;;
let is_int = Big_int.is_int_big_int;;
let to_int = Big_int.int_of_big_int;;
let of_int32 = Big_int.big_int_of_int32;;
let of_nativeint = Big_int.big_int_of_nativeint;;
let of_int64 = Big_int.big_int_of_int64;;
let to_int32 = Big_int.int32_of_big_int;;
let to_nativeInt = Big_int.nativeint_of_big_int;;
let to_int64 = Big_int.int64_of_big_int;;
let to_float = Big_int.float_of_big_int;;
let (&&) = Big_int.and_big_int;;
let (||) = Big_int.or_big_int;;
let xor = Big_int.xor_big_int;;
let shift_left = Big_int.shift_left_big_int;;
let shift_right = Big_int.shift_right_big_int;;
let shift_right_towards_zero = Big_int.shift_right_towards_zero_big_int;;
let extract = Big_int.extract_big_int;;
let to_nat = Big_int.nat_of_big_int;;
let of_nat = Big_int.big_int_of_nat;;
let power_base = Big_int.base_power_big_int;;
let of_string_sys = Big_int.sys_big_int_of_string;;
let round_futur_last_digit = Big_int.round_futur_last_digit;;
let approx = Big_int.approx_big_int;;
let print big_int = print_string (to_string big_int);;
let pow2 n = shift_left unit n;;

let gaussbino q r m =
	let rec aux = function
		| m when r=m -> unit
		| m -> (((pow q m)-unit)*(aux (m-unit)))/((pow q (m-r))-unit)
	in if r=zero
	then unit
	else if r=m
	then unit
	else if r>m
	then zero
	else aux m
;;

let log x =
	let rec aux = function
		| n when n=zero -> assert false
		| n when n < x	-> unit
		| n				-> unit+(aux (n/x))
	in
	let rec aux0 = function
		| n when n=zero -> assert false
		| n when n=unit	-> zero
		| n	when n < x	-> unit
		| n when modulo n x=zero -> unit+(aux0(n/x))
		| n				-> unit+(aux (n/x))
	in aux0
;;
