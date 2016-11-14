type ('a, 'b) t

val create : int -> ('a, 'b)t

val test : ('a, 'b) t -> 'a -> bool
val push : ('a, 'b) t -> 'a -> 'b -> unit
val memo : ('a, 'b) t -> 'a -> 'b -> 'b
val pull : ('a, 'b) t -> 'a -> 'b	

val apply : ('a, 'b) t -> ('a -> 'b) -> 'a -> 'b
	
val print_stats : ('a, 'b) t -> unit
