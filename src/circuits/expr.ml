(* Definition of type expr' : logical formulae in tree form *)
type uop = PNot | PNop
type bop = PAnd | POr | PIff | PImp | PXor
type 'a expr =
	| PCst of bool
	| PVar of 'a
	| PUop of uop * 'a expr
	| PBop of bop * 'a expr * 'a expr

let no x = PUop (PNop, x)
let (&!) x y = PBop (PAnd, x, y)
let (|!) x y = PBop (POr, x, y)
let cons c x0 x1 = ((no c)&!x0)|!(c&!x1)


type iformula = {
	iinput  : int array;
	ioutput : int array;
	iassign : int expr array;
}

let streedump_expr dump =
	let dump_uop = function
		| PNop -> Tree.Leaf "+"
		| PNot -> Tree.Leaf "-"
	in
	let dump_bop = function
		| PAnd -> Tree.Leaf "&"
		| POr  -> Tree.Leaf "|"
		| PIff -> Tree.Leaf "<->"
		| PImp -> Tree.Leaf "->"
		| PXor -> Tree.Leaf "^"
	in
	let rec eval = function
		| PCst cst -> Tree.Leaf (if cst then "true" else "false")
		| PVar var -> Tree.Leaf (dump var)
		| PUop (uop, expr) -> Tree.Node [dump_uop uop; eval expr]
		| PBop (bop, expr0, expr1) -> Tree.Node [dump_bop bop; eval expr0; eval expr1]
	in eval

let strdump_expr dump =
	let dump_uop = function
		| PNop ->  "+"
		| PNot ->  "-"
	in
	let dump_bop = function
		| PAnd ->  "&"
		| POr  ->  "|"
		| PIff ->  "<->"
		| PImp ->  "->"
		| PXor ->  "^"
	in
	let rec eval = function
		| PCst cst -> [if cst then "true" else "false"]
		| PVar var -> [dump var]
		| PUop (uop, expr) -> ["("]@[dump_uop uop]@(eval expr)@[")"]
		| PBop (bop, expr0, expr1) -> ["("]@(eval expr0)@[dump_bop bop]@(eval expr1)@[")"]
	in
	fun expr -> String.concat " " (eval expr)

let import_iformula
		(cst :  bool -> int -> 'edge)
		(var :   int -> 'edge array)
		((&!): 'edge -> 'edge -> 'edge)
		((^!): 'edge -> 'edge -> 'edge)
		(neg : 'edge -> 'edge)
		formula : 'edge array =
	let size = Array.length formula.iinput in
	let compute = Array.make ((Array.length formula.iassign)+size) None in
	let input = var size in
	for i = 0 to size-1 do compute.(i) <- (Some input.(i)); done;
	let rec eval_ident i = match compute.(i) with
		| Some edge -> edge
		| None ->
			let result = eval_expr (formula.iassign.(i-size)) in
			compute.(i) <- Some result;
			result
	and     eval_expr = function
		| PVar ident -> eval_ident ident
		| PUop (uop, expr) ->
		(
			let func = eval_expr expr in
			match uop with
			| PNop ->     func
			| PNot -> neg func 
		)
		| PBop (bop, exprX, exprY) ->
		(
			let funcX = eval_expr exprX
			and funcY = eval_expr exprY in
			match bop with
			| PAnd  -> funcX &! funcY
			| POr   -> neg((neg funcX)&!(neg funcY))
			| PImp	-> neg(funcX &! (neg funcY))
			| PIff	-> neg(funcX ^! funcY)
			| PXor	-> funcX ^! funcY
		)
		| PCst bool -> cst bool size
	in
	Array.map eval_ident formula.ioutput

type 'a formula = {
	input  : 'a array;
	output : 'a array;
	assign : ('a * ('a expr)) array;
}

let iformula_of_formula (formula:'a formula) : iformula =
	let ninput  = Array.length formula.input in
	let h2size  = (Array.length formula.assign)+ ninput in 
	let h2table = H2Table.create h2size 0 in
	let push    = H2Table.push  h2table
	and push'   = H2Table.push' h2table in
	let iinput  = Array.mapi (fun idx name -> assert(idx = push name); idx) formula.input in
	let assign  = Array.mapi (fun idx (name, expr) -> assert(idx+ninput = push name); expr) formula.assign in
	let ioutput = Array.map push' formula.output in
	let rec eval = function
		| PVar ident               -> PVar (push' ident)
		| PUop (uop, expr)         -> PUop (uop, eval expr)
		| PBop (bop, exprX, exprY) -> PBop (bop, eval exprX, eval exprY)
		| PCst bool                -> PCst bool
	in
	let iassign = Array.map eval assign in
	{iinput; ioutput; iassign}

let import_formula
		(cst :  bool -> int -> 'edge)
		(var :   int -> 'edge array)
		((&!): 'edge -> 'edge -> 'edge)
		((^!): 'edge -> 'edge -> 'edge)
		(neg : 'edge -> 'edge)
		(formula : 'a formula) : ('a * 'edge) array =
	let ioutput = import_iformula cst var (&!) (^!) neg (iformula_of_formula formula) in
	Array.map2 (fun name  edge -> (name, edge)) formula.output ioutput
