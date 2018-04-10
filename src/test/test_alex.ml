let nbit = int_of_string(Sys.argv.(1));;
let file_out = Sys.argv.(2);;
let man = CpB.TACX.G1.newman();;

module OOPS = CpB.OOPS(struct let tacx = man end);;
open OOPS.SUInt

let arity = 3 * nbit;;

let vars = OOPS.array_make_n_var arity;;
let x  = {arity; array = Array.init nbit (fun i -> vars.(3*i+0))}
let m  = {arity; array = Array.init nbit (fun i -> vars.(3*i+1))}
let m' = {arity; array = Array.init nbit (fun i -> vars.(3*i+2))};;

let eqL = (x |&/ m) +/ (x |&/ m');;
let eqR = ( x |&/ (m |^/ m')) +/ (( x |&/ m |&/ m') <</ 1);;
let edge = eqL =/ eqR;;

(*
let edges = (Array.to_list vars)@(Array.to_list eqL.array)@(Array.to_list eqR.array)@[edge] in
*)
CpB.TACX.dumpfile man [OOPS.neg edge] file_out;;

exit 0;;
