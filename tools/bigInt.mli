type big_int
val zero : big_int
val unit : big_int
val minus : big_int -> big_int
val abs : big_int -> big_int
val ( + ) : big_int -> big_int -> big_int
val succ : big_int -> big_int
val addi : int -> big_int -> big_int
val ( - ) : big_int -> big_int -> big_int
val pred : big_int -> big_int
val ( * ) : big_int -> big_int -> big_int
val multi : int -> big_int -> big_int
val square : big_int -> big_int
val sqrt : big_int -> big_int
val quomod : big_int -> big_int -> big_int * big_int
val ( / ) : big_int -> big_int -> big_int
val modulo : big_int -> big_int -> big_int
val gcd : big_int -> big_int -> big_int
val pow_ii : int -> int -> big_int
val pow_bi : big_int -> int -> big_int
val pow_ib : int -> big_int -> big_int
val pow : big_int -> big_int -> big_int
val pow2 : int -> big_int
val sign : big_int -> int
val compare : big_int -> big_int -> int
val ( = ) : big_int -> big_int -> bool
val ( <= ) : big_int -> big_int -> bool
val ( >= ) : big_int -> big_int -> bool
val ( < ) : big_int -> big_int -> bool
val ( > ) : big_int -> big_int -> bool
val max : big_int -> big_int -> big_int
val min : big_int -> big_int -> big_int
val len : big_int -> int
val to_string : big_int -> string
val of_string : string -> big_int
val of_int : int -> big_int
val is_int : big_int -> bool
val to_int : big_int -> int
val of_int32 : int32 -> big_int
val of_nativeint : nativeint -> big_int
val of_int64 : int64 -> big_int
val to_int32 : big_int -> int32
val to_nativeInt : big_int -> nativeint
val to_int64 : big_int -> int64
val to_float : big_int -> float
val ( && ) : big_int -> big_int -> big_int
val ( || ) : big_int -> big_int -> big_int
val xor : big_int -> big_int -> big_int
val shift_left : big_int -> int -> big_int
val shift_right : big_int -> int -> big_int
val shift_right_towards_zero : big_int -> int -> big_int
val extract : big_int -> int -> int -> big_int
val to_nat : big_int -> Nat.nat
val of_nat : Nat.nat -> big_int
val power_base : int -> int -> big_int -> big_int
val of_string_sys : string -> int -> int -> big_int
val round_futur_last_digit : string -> int -> int -> bool
val approx : int -> big_int -> string
val print : big_int -> unit
(*gaussbino q(gaussian parameter) r(subset size) m(superset size)*)
val gaussbino : big_int -> big_int -> big_int -> big_int
(*log x(basis) n *)
val log : big_int -> big_int -> big_int 
