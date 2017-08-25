open O3

let from_a_bc_de_to_abd_c_e : (('a*('b*'c)*('d*'e)), (('a*'b*'d)*'c*'e)) o3 =
(
	(fun (a, (b, c), (d, e)) -> ((a, b, d), c, e)),
	(fun ((a, b, d), c, e) -> (a, (b, c), (d, e)))
)
