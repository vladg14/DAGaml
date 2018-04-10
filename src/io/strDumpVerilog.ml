open IoUtils

let strdump_expr expr =
	RExpr.strdump
		(fun x -> x)
		(function true -> "1'b1" | false -> "1'b0")
		RExpr.(function RAnd -> " & " | RXor -> " ^ ")
		"~"
		(RExpr.rexpr_of_expr expr)

let dump (output : string -> unit) (file:module_expr) =
	output ("module "^(file.expr_name)^" (");
	output (String.concat ", " file.expr_param);
	output ")";
	if file.expr_input <> []
	then (
		output ";\n\tinput ";
		output (String.concat ", " file.expr_input)
	);
	output ";\n\toutput ";
	output (String.concat ", " file.expr_output);
	if file.expr_wire <> []
	then (
		output ";\n\twire ";
		output (String.concat ", " file.expr_wire)
	);
	output ";\n";
	let dump_assign (name, expr) =
		output ("\tassign "^name^" = "^(strdump_expr expr)^";\n")
	in	
	List.iter dump_assign file.expr_assign;
	output "endmodule\n"

let dump_file file mexpr =
	let fout = open_out file in
	dump (output_string fout) mexpr;
	close_out fout
