type 'a t
(**
create hsize index
@ hsize : the size of the hash-table
@ index : index of the first element
**)
val create : int -> int -> 'a t
val memA : 'a t -> 'a -> bool
val memI : 'a t -> int -> bool
val push : 'a t -> 'a -> int
val pull : 'a t -> int -> 'a
val length : 'a t -> int
val iter : 'a t -> ('a -> int -> unit) -> unit
