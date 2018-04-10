module T = Cpx2B

let conv_stree_file stree fileB args =
	let tacx, edges = T.TACX.stree_load stree in
	let cout = open_out fileB in
	output_string cout "tacx system:";
	output_string cout (if T.TACX.check tacx edges
		then "checked"
		else "error");
	output_string cout "\n";
	close_out cout
