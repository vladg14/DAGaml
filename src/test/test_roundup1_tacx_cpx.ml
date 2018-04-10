assert(Array.length Sys.argv > 2);
(* TODO: display --help *)

let file_in = Sys.argv.(1) in
let file_out = Sys.argv.(2) in

print_string "load tacx : start"; print_newline();
let tacx, edges = Cpx.TACX.loadfile file_in in
print_string "load tacx : done "; print_newline();
let pman, propa = Cpx.TACX_PROPA.newman tacx in
let peval = Cpx.TACX_PROPA.eval pman in
let ( *! ) = Cpx.( *! ) tacx in

let roundup edge =
	let n = CpxUtils.node_size edge in
	if n = 0
	then edge
	else
	(
		let set0 = (MyList.make (n-1) None)@[Some false]
		and set1 = (MyList.make (n-1) None)@[Some true ] in
		assert(n = List.length set0);
		assert(n = List.length set1);
		let edge0 = peval set0 edge
		and edge1 = peval set1 edge in
		edge0 *! edge1
	)
in

print_string "propa : start"; print_newline();
let edges = List.map propa edges in
print_string "propa : done"; print_newline();
STree.pprint [Cpx.TACX.dump_stats tacx; Cpx.TACX_PROPA.dump_stats pman];
print_string "roundup : start"; print_newline();
let edges = List.map roundup edges in
print_string "roundup : done"; print_newline();
print_string "propa : start"; print_newline();
let edges = List.map propa edges in
print_string "propa : done"; print_newline();
STree.pprint [Cpx.TACX.dump_stats tacx; Cpx.TACX_PROPA.dump_stats pman];

Cpx.TACX.dumpfile tacx edges file_out;

if(Array.length Sys.argv > 3)
then
(
    let strman = Udag.String.newman() in
    let stredges = Cpx.TACX.to_dot tacx strman edges in
    Udag.String.to_dot_file strman stredges Sys.argv.(3)
);

exit 0;
