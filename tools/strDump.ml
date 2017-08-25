let ignore _ = "_"
let bitv = Bitv.L.to_bool_string
let option dump = function
	| None -> "None"
	| Some x -> "Some ("^(dump x)^")"
let list dump list = "["^(StrUtil.catmap "; " dump list)^"]"
let unit () = "()"
let bool = function true -> "true" | false -> "false"
let int = string_of_int
let pair dumpA dumpB (a, b) =
	"("^(dumpA a)^", "^(dumpB b)^")"
let trio dumpA dumpB dumpC (a, b, c) =
	"("^(dumpA a)^", "^(dumpB b)^", "^(dumpC c)^")"
