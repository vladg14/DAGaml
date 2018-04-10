open CpxTypes
open CpxUtils
open CpxDumpLoad
open CpxGops

let x = Extra.(StrLoad.bitv_hexa >> binload_node ) Sys.argv.(1);;
let x0, x1 = block_split x;;
let x01 = solve_cons (fun x -> x) (x0, Utils.Leaf()) (x1, Utils.Leaf());;
assert false
