open ConvTypes
let extern_tag _ = None
let input_tag = function
| "v"   -> Some Verilog
| "cnf" -> Some Cnf
| "pla" -> Some Pla
| _     -> None
let output_tag = function
| "dot"    -> Some Dot
| "check"  -> Some Check
| "stats"  -> Some Stats
| "cntsat" -> Some CntSat
| "allsat" -> Some AllSat
| _ -> None
let modele_tag = function
| "bryant" -> Some Bryant
| "zdd"    -> Some Zdd
| "cp"     -> Some Cp
| "nni"    -> Some Nni
| "cpx"    -> Some Cpx
| _ -> None
let tacx_pure = function
| "tacx"   -> Some Tacx
| "pure"   -> Some Pure
| _ -> None
let version_tag = function
| "A" -> Some VA
| "B" -> Some VB
| _ -> None
let file_tag = function
| [x] ->
(
	match extern_tag x with
	| Some y -> Ext y
	| None -> match input_tag x with
	| Some y -> ExtI y
	| None -> match output_tag x with
	| Some y -> ExtO y
	| None -> assert false
)
| [y; z; x] ->
( match tacx_pure x, modele_tag y, version_tag z with
	| Some x, Some y, Some z -> Int(x, y, z)
	| _ -> assert false
)
| _ -> assert false

let file_ext string = match StrUtil.split '.' string with
	| ""::tail -> file_tag tail
	| _ -> failwith ("conv/convTypesLoad:file_ext - error parsing : \""^(string)^"\"")
