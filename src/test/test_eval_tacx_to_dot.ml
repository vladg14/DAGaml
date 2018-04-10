let file_in = Sys.argv.(2) in
let file_out = Sys.argv.(3) in
let strman = Udag.String.newman () in

let stredges = match Sys.argv.(1) with
| "--nni" ->
(
	let tacx, edges = Nni.TACX.loadfile file_in in
	Nni.TACX.to_dot tacx strman edges
)
| "--cp" ->
(
	let tacx, edges = Cp.TACX.loadfile file_in in
	Cp.TACX.to_dot tacx strman edges
)
| "--cpx" ->
(
	let tacx, edges = Cpx.TACX.loadfile file_in in
	Cpx.TACX.to_dot tacx strman edges
)
| _ -> assert false
in
Udag.String.to_dot_file strman stredges file_out;

exit 0;
