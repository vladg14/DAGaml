open Extra

type ('a, 'b) o3  = ('a -> 'b) * ('b -> 'a)
type ('a, 's) o3s = ('a -> 's -> 's ) * ('s -> 'a * 's)

let id = ((fun x -> x), (fun x -> x))

let o3_check (a_b, b_a) =
(
	(fun a ->
		let b  = a_b a in
		let a' = b_a b in
		assert(a = a');
		b
	),
	(fun b ->
		let a  = b_a b in
		let b' = a_b a in
		assert(b = b');
		a
	)
)

let o3s_check (dump, load) =
(
	(fun a s ->
		let s' = dump a s in
		let a', s'' = load s' in
		assert(a = a' && s = s'');
		s'
	),
	(fun s ->
		let a, s' = load s in
		let s'' = dump a s' in
		assert(s = s'');
		(a, s')
	)
)

let o3_o3 (a_b, b_a) (b_c, c_b) = Extra.(a_b >> b_c, c_b >> b_a)

let ( %>> ) = o3_o3

let o3_o3s ((a_b, b_a) : ('a, 'b) o3) ((dump, load) : ('b, 's) o3s) : ('a, 's) o3s =
(
	(fun a s -> dump (a_b a) s),
	(fun   s ->
		let b, s = load s in
		(b_a b, s)
	)
)

let ( +>> ) = o3_o3s

let o3s_o3 (dump, load) (s_s', s'_s) =
(
	(fun a s' -> s_s' (dump a (s'_s s'))),
	(fun   s' ->
		let a, s = load (s'_s s') in
		(a, s_s' s)
	)
)

let ( >>+ ) = o3s_o3

let cat (a_a', a'_a) (b_b', b'_b) =
(
	(fun (a, b) -> (a_a' a, b_b' b)),
	(fun (a', b') -> (a'_a a', b'_b b'))
)

let ( %* ) = cat

let ( +* ) (dumpA, loadA) (dumpB, loadB) =
(
	(fun (a, b) s -> dumpA a (dumpB b s)),
	(fun s ->
		let a, s = loadA s in
		let b, s = loadB s in
		(a, b), s
	)
)

let pair (o3A, o3B) = o3A +* o3B

let trio (((dumpA, loadA), (dumpB, loadB), (dumpC, loadC)) : ('a, 's) o3s * ('b, 's) o3s * ('c, 's) o3s ) : ('a * 'b * 'c, 's) o3s =
(
	(fun (a, b, c) s -> dumpA a (dumpB b (dumpC c s))),
	(fun s ->
		let a, s = loadA s in
		let b, s = loadB s in
		let c, s = loadC s in
		(a, b, c), s
	)
)

let closure e ((dump, load) : ('a, 's) o3s) : ('a, 's) o3 =
(
	(fun a -> dump a e),
	(fun s ->
		let a, s = load s in
		assert(s = e);
		a
	)
)
