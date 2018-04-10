open Expr

type module_expr = {
	expr_name   : string;
	expr_param  : string list;
	expr_input  : string list;
	expr_output : string list;
	expr_wire   : string list;
	expr_assign : (string * (string expr)) list;
}

let module_expr_of_iformula
		(expr_name : string)
		(string_of_input  : int -> string)
		(string_of_wire   : int -> string)
		(string_of_output : int -> string)
		(form : iformula) : module_expr =
	Array.iteri (fun idx var -> assert(idx = var)) form.iinput;
	(* TODO : redefine iformula with this assertion in mind *)
	let ninput = Array.length form.iinput in
	let noutput = Array.length form.ioutput in
	let nassign = Array.length form.iassign in
	assert(nassign >= noutput);
	let nwire = nassign - noutput in
	let nvars = nassign + ninput  in
	let names = Array.make nvars None in
	let expr_input  : string array = Array.init ninput (fun idx ->
		let name = string_of_input  idx in
		assert(name <> "false");
		names.(idx) <- Some name;
		name)
	and expr_output : string array = Array.init noutput (fun idx ->
		let name = string_of_output idx in
		assert(name <> "false");
		name)
	and expr_wire   : string array = Array.init nwire (fun idx ->
		let name = string_of_wire   idx in
		assert(name <> "false");
		name)
	in
	(* define outputs' name properly *)
	Array.iteri
		(fun idx var -> names.(var) <- Some expr_output.(idx))
		form.ioutput;
	let wire_cnt = ref 0 in
	let click () =
		let idx = !wire_cnt in
		wire_cnt := 1 + !wire_cnt;
		idx
	in
	let filter = function
		| Some name -> name
		| None -> expr_wire.(click())
	in
	(* define wires' name properly *)
	let names = Array.map filter names in
	let rec eval = function
		| PCst cst -> PCst cst
		| PVar var -> PVar names.(var)
		| PUop (uop, expr) -> PUop (uop, eval expr)
		| PBop (bop, expr0, expr1) -> PBop (bop, eval expr0, eval expr1)
	in
	let expr_assign = Array.mapi
		(fun idx expr -> (names.(idx+ninput), eval expr))
		form.iassign
	in
	let expr_input  = Array.to_list expr_input
	and expr_output = Array.to_list expr_output
	and expr_wire   = Array.to_list expr_wire
	and expr_assign = Array.to_list expr_assign in
	{
		expr_name;
		expr_param = expr_input @ expr_output;
		expr_input; 
		expr_output;
		expr_wire; 
		expr_assign; 
	}

let import_expr
		(cst :  bool -> int -> 'edge)
		(var :   int -> 'edge array)
		((&!): 'edge -> 'edge -> 'edge)
		((^!): 'edge -> 'edge -> 'edge)
		(neg : 'edge -> 'edge)
		(mymodule:module_expr) : (string * 'edge) list =
	let formula = {
		input  = Array.of_list mymodule.expr_input;
		output = Array.of_list mymodule.expr_output;
		assign = Array.of_list mymodule.expr_assign;
	} in
	Array.to_list (import_formula cst var (&!) (^!) neg formula)

type module_tacx = {
	tacx_name : string;
	tacx_input: string list;
	tacx_man  : Cp.TACX.manager;
	tacx_edges: (string * Cp.TACX.edge) list;
}

let module_tacx_stree_dump tacx =
	Cp.TACX.stree_dump tacx.tacx_man (List.map snd tacx.tacx_edges)

let module_tacx_of_module_expr module_expr =
	let tacx_man   = Cp.TACX.newman() in
	let tacx_edges = import_expr
		(fun cst size -> ((cst, MyList.make size (CpTypes.P)), Utils.Leaf ()))
		(Oops.array_make_n_var tacx_man)
		(Cp.(&!) tacx_man)
		(Cp.(^!) tacx_man)
		Cp.no
		module_expr
	in {
		tacx_name = module_expr.expr_name;
		tacx_man;
		tacx_input= module_expr.expr_input;
		tacx_edges;
	}

type module_tacxB = {
	tacxB_name : string;
	tacxB_input: string list;
	tacxB_man  : CpB.TACX.G1.manager;
	tacxB_edges: (string * CpB.TACX.G0.edge') list;
}

let rec tee stream =
	let rec aux x =
		try
		(
			let char = Stream.next stream in
			print_char char; flush stdout;
			Some char
		)
		with _ -> None
	in Stream.from aux


let module_expr_of_nax
		(module_name : string)
		(ninputs : int)
		?(string_of_input  : int -> string = fun x -> "x"^(string_of_int x))
		?(string_of_wire   : int -> string = fun x -> "w"^(string_of_int x))
		?(string_of_output : int -> string = fun x -> "y"^(string_of_int x))
		(nax : Nax.NAX.G.manager)
		(outputs : Nax.NAX.G.edge' array) : module_expr =
	let iform = Nax.TO_IFormula.export nax ninputs outputs in
	module_expr_of_iformula
		module_name
		string_of_input
		string_of_wire
		string_of_output
		iform

let default_string_of_input  : int -> string = fun x -> "x"^(string_of_int x)
let default_string_of_wire   : int -> string = fun x -> "w"^(string_of_int x)
let default_string_of_output : int -> string = fun x -> "y"^(string_of_int x)

let cnax_of_module_expr mexpr =
	let man = Cnax.CNAX.G1.newman() in
	let push tag x y = Cnax.CNAX.G1.push man (tag, x, y) in
	let edges = import_expr
		(fun cst size -> (cst, Utils.Leaf None))
		(fun     size -> Array.init size (fun x -> (false, Utils.Leaf(Some x))))
		(fun edge0 edge1 -> push Cnax.And edge0 edge1)
		(fun edge0 edge1 -> push Cnax.Xor edge0 edge1)
		(fun (neg, node) -> (not neg, node))
		mexpr
	in (man, edges)

let module_expr_of_cnax
		(module_name : string)
		(ninputs : int)
		?(string_of_input  = default_string_of_input )
		?(string_of_wire   = default_string_of_wire  )
		?(string_of_output = default_string_of_output)
		(cnax : Cnax.CNAX.G0.manager)
		(outputs : Cnax.CNAX.G0.edge' array) : module_expr =
	let nax = Nax.NAX.newman() in
	let tonax = Cnax.CNAX.TO_NAX.newman cnax nax in
	let apply = Cnax.CNAX.TO_NAX.rec_edge tonax in
	module_expr_of_nax
		module_name
		ninputs
		~string_of_input:string_of_input
		~string_of_wire:string_of_wire
		~string_of_output:string_of_output
		nax
		(Array.map apply outputs)
