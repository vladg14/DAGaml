(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

type elem =
	| S (* significant variable *)
	| P (* useless variable *)
	| X of (bool * int) (* if x then (i [2] XOR shift XOR neg) else ... *)

type spx_tag = {hasS : bool; hasP : bool; maxX : int option}

type block_spx = bool * spx_tag * (elem list)

type block_choice =
	| C0
	| Id  of int
	| SPX of block_spx

type block = {
	neg		: bool;
	arity	: int;
	block	: block_choice;
}

type 'a next = (unit, 'a) Utils.gnode
type 'a edge = block * 'a next
type 'a node = unit          * 'a edge * 'a edge
type 'a tacx = TacxTypes.tag * 'a edge * 'a edge

type    peval  = GUtils.peval
type   opeval  = peval option
type 'a plink  = opeval * 'a
type 'a pedge  = 'a plink edge
type 'a pnode  = 'a plink node

type 'a merge   = ('a next, 'a node) Utils.merge
type 'a emerge  = block * 'a merge
type 'a merge3  = ('a next, 'a node, 'a node) Utils.merge3
type 'a emerge3 = block * 'a merge3

type    quant  = GUtils.quant
type  opquant  = quant option
type 'a qlink  = opquant * 'a
type 'a qedge  = 'a qlink edge
type 'a qnode  = 'a qlink node
