open Expr

(* Definition of type expr : flattened expressions *)
type rbop = RAnd | RXor
(*	RExpr (RBop (RAnd, [])) = RCst true
		RExpr (RBop (RXor, [])) = RCst false
*)
type 'a aexpr =
	| RVar of 'a
	| RBop of rbop * ('a bexpr list)
(* [true] = neg | [false] = id *)
and  'a bexpr = bool * 'a aexpr
type 'a rexpr =
	| RCst of bool
	| RExpr of 'a bexpr;;


(* Conversion function from expr to rexpr *)

(* Gives the negation of a rexpr *)
let no (re : 'a rexpr) : 'a rexpr =
	match re with
	| RCst b -> RCst (not b)
	| RExpr (b, a) -> RExpr (not b, a)

let (&!) (re1 : 'a rexpr) (re2 : 'a rexpr) : 'a rexpr =
	match re1, re2 with
	| RCst false, _
	| _, RCst false -> RCst false
	| RCst true, re
	| re, RCst true -> re
	| RExpr (false, RBop(RAnd, l1)), RExpr (false, RBop(RAnd, l2)) ->
		RExpr (false, RBop(RAnd, l1@l2))
	| RExpr (false, RBop(RAnd, l)), RExpr be
	| RExpr be, RExpr (false, RBop(RAnd, l)) ->
		RExpr (false, RBop(RAnd, be::l))
	| RExpr be1, RExpr be2 -> RExpr (false, RBop (RAnd, [be1;be2]))


let (^!) (re1 : 'a rexpr) (re2 : 'a rexpr) : 'a rexpr =
	match re1, re2 with
	| RCst false, re
	| re, RCst false -> re
	| RCst true, re
	| re, RCst true -> no re
	| RExpr (b1, RBop(RXor, l1)), RExpr (b2, RBop(RXor, l2)) ->
		RExpr (b1<>b2, RBop(RXor, l1@l2))
	| RExpr (b, RBop(RXor, l)), RExpr (b', ae)
	| RExpr (b', ae), RExpr (b, RBop(RXor, l)) ->
		RExpr (b<>b', RBop(RXor, (false, ae)::l))
	| RExpr (b1, ae1), RExpr (b2, ae2) ->
		RExpr (b1<>b2, RBop (RXor, [(false, ae1);(false, ae2)]))

let strdump dump_var dump_cst dump_bop dumped_neg =
	let rec eval_expr  = function
		| RVar var           -> dump_var var
		| RBop (rbop, liste) ->
			("( "^(String.concat (dump_bop rbop) (List.map eval_bexpr liste))^" )")
	and     eval_bexpr (neg, expr) =
		(if neg then dumped_neg else "")^(eval_expr expr)
	in
	let eval_expr' = function
		| RVar var           -> dump_var var
		| RBop (rbop, liste) ->
			(String.concat (dump_bop rbop) (List.map eval_bexpr liste))
	in
	let eval_bexpr' (neg, expr) = if neg
		then (dumped_neg^"( "^(eval_expr expr)^" )")
		else (eval_expr' expr)
	in
	let eval_rexpr = function
		| RCst cst    -> dump_cst cst
		| RExpr bexpr -> eval_bexpr' bexpr
	in eval_rexpr

(* Definition : alias for Or, Imp, Iff *)

let (|!) (re1 : 'a rexpr) (re2 : 'a rexpr) : 'a rexpr =
	no ((no re1) &! (no re2))

let (->!) (re1 : 'a rexpr) (re2 : 'a rexpr) : 'a rexpr =
	no (    re1  &! (no re2))

let (=!) (re1 : 'a rexpr) (re2 : 'a rexpr) : 'a rexpr =
	no (    re1  ^!     re2 )

(* Definition : main conversion function *)


let rexpr_of_expr (e : 'a expr) : 'a rexpr =
	let rec convert e =
		match e with
		| PCst b -> RCst b
		| PVar v -> RExpr (false, RVar v)
		| PUop (PNop, e') -> convert e'
		| PUop (PNot, e') -> no (convert e')
		| PBop (PAnd, e1, e2) -> (convert e1) &!  (convert e2)
		| PBop (POr , e1, e2) -> (convert e1) |!  (convert e2)
		| PBop (PIff, e1, e2) -> (convert e1) =!  (convert e2)
		| PBop (PImp, e1, e2) -> (convert e1) ->! (convert e2)
		| PBop (PXor, e1, e2) -> (convert e1) ^!  (convert e2)
	in convert e
