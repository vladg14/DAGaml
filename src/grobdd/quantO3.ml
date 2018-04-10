open O3Extra
open BinO3
open Quant

let o3_quant : (quant, bool * int * (int option list)) O3.o3 =
(
	(fun quant -> (quant.neg, quant.arity, quant.block)),
	(fun (neg, arity, block) -> {neg; arity; block})
)

let o3s_quant = o3_quant +>> O3.trio (bool, int, list (option int))

let strdump_quant quant =
	StrDump.("{neg = "^(bool quant.neg)^"; arity = "^(int quant.arity)^"; block = "^(list (option int) quant.block)^"}")
