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

type block2 = block * block

type edge_state = block
type node_state = block2
type node_and_state = block2
type node_xor_state = block2
type tacx_state = TacxTypes.tag * block2

type 'a edge = edge_state * (unit, 'a) Utils.gnode
type 'a node = node_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode
type 'a tacx = tacx_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode

type 'a next' = (unit, 'a) Utils.gnode
type 'a edge' = edge_state * 'a next'
type 'a node' = unit          * 'a edge' * 'a edge'
type 'a tacx' = TacxTypes.tag * 'a edge' * 'a edge'

type    peval  = bool option list option
type 'a pedge  = (peval * 'a) edge
type 'a pedge' = (peval * 'a) edge'
type 'a pnode  = (peval * 'a) node
type 'a pnode' = (peval * 'a) node'

type 'a merge   = ('a next', 'a node') Utils.merge
type 'a emerge  = edge_state * 'a merge
type 'a merge3  = ('a next', 'a node', 'a node') Utils.merge3
type 'a emerge3 = edge_state * 'a merge3
