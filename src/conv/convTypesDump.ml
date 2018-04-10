open ConvTypes
let extern_tag () = failwith "extern_tag is empty"
let input_tag = function
| Verilog -> "v"
| Cnf     -> "cnf"
| Pla			-> "pla"
let output_tag = function
| Dot			-> "dot"
| Check		-> "check"
| Stats		-> "stats"
| CntSat	-> "cntsat"
| AllSat	-> "allsat"
let modele_tag = function
| Bryant	-> "bryant"
| Zdd			-> "zdd"
| Cp			-> "cp"
| Nni			-> "nni"
| Cpx			-> "cpx"
let tacx_pure = function
| Tacx		-> "tacx"
| Pure		-> "pure"
let version_tag = function
| VA			-> "A"
| VB			-> "B"
let file_tag = function
| Ext  x -> [extern_tag x]
| ExtI x -> [input_tag x]
| ExtO x -> [output_tag x]
| Int  (x, y, z) ->
	[modele_tag y; version_tag z; tacx_pure x]
let file_ext x = "."^(String.concat"."(file_tag x))
