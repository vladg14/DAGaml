type 'a o3b = ('a, Bitv.t) O3.o3
type 'a o3s = ('a, BinUtils.stream) O3.o3s 

let unit = (BinDump.unit, BinLoad.unit)
let option (dump, load) = (BinDump.option dump, BinLoad.option load)
let bool = (BinDump.bool, BinLoad.bool)
let bitv = (BinDump.bitv, BinLoad.bitv)
let list (dump, load) = (BinDump.list dump, BinLoad.list load)
let int = (BinDump.int, BinLoad.int)
let closure ((dump, load) : ('a, 's) O3.o3s) : ('a, Bitv.t) O3.o3 =
	(BinDump.closure dump, BinLoad.closure load)
let bool_option_list = (BinDump.bool_option_list, BinLoad.bool_option_list)
