type 'a iter
type 'a next = 'a iter -> 'a iter

val range	: int -> int -> int iter

val map		: ('a -> 'b) -> 'a iter -> 'b iter
val ( $$  )	: 'a iter -> ('a -> 'b) -> 'b iter
val ( $? )  : 'a iter -> ('a -> 'b option) -> 'b iter

val map'	: ('a -> 'b) -> 'a iter -> 'b next
val ( $$+ )	: 'a iter -> ('a -> 'b) -> 'b next

val fold_left0	: ('a -> 'b -> 'a) -> 'a -> 'b iter -> 'a
val ( $!  )	: 'a iter -> ('b -> 'a -> 'b) -> 'b -> 'b
val fold_left	: ('a -> 'a -> 'a) -> 'a iter -> 'a option
val ( $!! )	: 'a iter -> ('a -> 'a -> 'a) -> 'a option

val of_list : 'a list -> 'a iter

val iter	: ('a -> unit) -> 'a iter -> unit
val count	: bool iter -> int
val length	: 'a iter -> int
val stop	: 'a iter
val is_stop	: 'a iter -> bool
val pull	: 'a iter -> ('a * 'a iter) option
val push	: 'a -> 'a iter -> 'a iter
val fill_None	: 'a option iter -> 'a iter -> 'a iter
val fill_None_partial	: 'a option iter -> 'a option iter -> 'a option iter
val fill_None_default	: 'a -> 'a option iter -> 'a iter
val zip	: 'a iter -> 'b iter -> ('a * 'b) iter
val enumerate	: int -> 'a iter -> (int * 'a) iter
val progress : string -> int -> int -> 'a iter -> 'a iter
val iter_while	: ('a -> 'a option) -> 'a -> 'a iter
val iter_fold	: ('a -> ('a * 'b) option) -> 'a -> 'b iter
val unop	: 'a option iter -> 'a iter

val filter' : ('a -> bool) -> 'a iter -> 'a next
val filter  : ('a -> bool) -> 'a iter -> 'a iter
val bool_a_pair_to_option' : (bool * 'a) iter -> 'a option next
val bool_a_pair_to_option  : (bool * 'a) iter -> 'a option iter
val pair_filter' : (bool * 'a) iter -> 'a next
val pair_filter  : (bool * 'a) iter -> 'a iter
val inv_filter' : bool iter -> 'a iter -> 'a option next
val inv_filter  : bool iter -> 'a iter -> 'a option iter

val to_list	: 'a iter -> 'a list
val to_list_partial : int -> 'a iter -> 'a list * 'a iter
val ( $+ )	: 'a iter -> 'a iter -> 'a iter
val add_fst'	: 'a -> 'b iter -> ('a * 'b) next
val add_fst	: 'a -> 'b iter -> ('a * 'b) iter
val add_snd'	: 'a iter -> 'b -> ('a * 'b) next
val add_snd	: 'a iter -> 'b -> ('a * 'b) iter
val compose'':('a -> 'b next) -> 'a iter -> 'b next
val ( $@++) : 'a iter -> ('a -> 'b next) -> 'b next
val ( $<++) :   int   -> (int -> 'b next) -> 'b next
val compose': ('a -> 'b iter) -> 'a iter -> 'b next
val ( $@+ )	: 'a iter -> ('a -> 'b iter) -> 'b next
val ( $<+ ) :   int   -> (int -> 'b iter) -> 'b next
val compose	: ('a -> 'b iter) -> 'a iter -> 'b iter
val ( $@ )	: 'a iter -> ('a -> 'b iter) -> 'b iter
val ( $< )  :   int	  -> (int -> 'b iter) -> 'b iter
val map_compose'	: ('a -> 'b -> 'c) -> 'a iter -> 'b iter -> 'c next
val map_compose		: ('a -> 'b -> 'c) -> 'a iter -> 'b iter -> 'c iter
val ( $*+ )	: 'a iter -> 'b iter -> ('a * 'b) next
val ( $* )	: 'a iter -> 'b iter -> ('a * 'b) iter
val add_tail'	: 'a iter -> 'a list -> 'a list iter -> 'a list iter
val add_tail		: 'a iter -> 'a list -> 'a list iter
val add_head'	: 'a -> 'a list iter -> 'a list iter -> 'a list iter
val add_head		: 'a -> 'a list iter -> 'a list iter
val ( $:+ )	: 'a iter -> 'a list iter -> 'a list iter -> 'a list iter
val ( $: )	: 'a iter -> 'a list iter -> 'a list iter
val inlist'	: 'a iter -> 'a list iter -> 'a list iter
val inlist	: 'a iter -> 'a list iter
val ( $^+ )	: 'a iter -> int -> 'a list iter -> 'a list iter
val ( $^ )	: 'a iter -> int -> 'a list iter
val eval	: 'a iter -> 'a iter
val half_square	: 'a iter -> int -> 'a list iter
val half_square_strict	: 'a iter -> int -> 'a list iter

val gen_bool : bool iter
