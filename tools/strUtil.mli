val char_of_bool : bool -> char
val bool_of_char : char -> bool
val string_of_bool : bool -> string
val bool_of_string : string -> bool
val pretty_of_bool : bool -> string
val print_bool : bool -> unit

val char_0 : int
val char_9 : int
val char_A : int
val char_Z : int
val char_a : int
val char_z : int

val explode: string -> char list
val implode: char list -> string
val catmap:  string -> ('a -> string) -> 'a list -> string

val index: string -> char -> int option
val index_from: string -> int -> char -> int option
val split: char -> string -> string list

val ntimes : string -> int -> string
val print_stream : bool list -> unit
