type 'a iter = 'a Iter.iter	
type 'a next = 'a Iter.iter -> 'a Iter.iter
	
let ( $$   ) = (Iter.( $$ )  : 'a iter -> ('a -> 'b) -> 'b iter			)
let ( $?   ) = (Iter.( $? )  : 'a iter -> ('a -> 'b option) -> 'b iter	)
let ( $$+  ) = (Iter.( $$+ ) : 'a iter -> ('a -> 'b) -> 'b next			)
let ( $!   ) = (Iter.( $! )  : 'a iter -> ('b -> 'a -> 'b) -> 'b -> 'b	)
let ( $!!  ) = (Iter.( $!! ) : 'a iter -> ('a -> 'a -> 'a) -> 'a option	)
let ( $+   ) = (Iter.( $+ )  : 'a iter -> 'a iter -> 'a iter			)
let ( $@++ ) = (Iter.( $@++) : 'a iter -> ('a -> 'b next) -> 'b next	)
let ( $@+  ) = (Iter.( $@+ ) : 'a iter -> ('a -> 'b iter) -> 'b next	)
let ( $@   ) = (Iter.( $@ )	 : 'a iter -> ('a -> 'b iter) -> 'b iter	)
let ( $*+  ) = (Iter.( $*+ ) : 'a iter -> 'b iter -> ('a * 'b) next		)
let ( $*   ) = (Iter.( $* )	 : 'a iter -> 'b iter -> ('a * 'b) iter		)
let ( $:+  ) = (Iter.( $:+ ) : 'a iter -> 'a list iter -> 'a list next	)
let ( $:   ) = (Iter.( $: )	 : 'a iter -> 'a list iter -> 'a list iter	)
let ( $^+  ) = (Iter.( $^+ ) : 'a iter -> int -> 'a list next			)
let ( $^   ) = (Iter.( $^ )	 : 'a iter -> int -> 'a list iter			)
let ( $<++ ) = (Iter.( $<++ ):   int   -> (int -> 'a next) -> 'a next	)
let ( $<+  ) = (Iter.( $<+ ) :   int   -> (int -> 'a iter) -> 'a next	)
let ( $<   ) = (Iter.( $<  ) :   int   -> (int -> 'a iter) -> 'a iter	)

