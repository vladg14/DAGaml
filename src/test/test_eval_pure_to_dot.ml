let file_in = Sys.argv.(2) in
let file_out = Sys.argv.(3) in
let strman = Udag.String.newman () in

let stredges = match Sys.argv.(1) with
| "--nni" ->
(
	let pure, edges = Nni.GroBdd.loadfile file_in in
	Nni.GroBdd.to_dot pure strman edges
)
| "--cp" ->
(
	let pure, edges = Cp.GroBdd.loadfile file_in in
	Cp.GroBdd.to_dot pure strman edges
)
| "--cpx" ->
(
	let pure, edges = Cpx.GroBdd.loadfile file_in in
	Cpx.GroBdd.to_dot pure strman edges
)
| "--bryant" ->
(
	let pure, edges = Bryant.GroBdd.loadfile file_in in
	Bryant.GroBdd.to_dot pure strman edges
)
| "--zdd" ->
(
	let pure, edges = Zdd.GroBdd.loadfile file_in in
	Zdd.GroBdd.to_dot pure strman edges
)
| _ -> assert false
in
Udag.String.to_dot_file strman stredges file_out;

exit 0;
