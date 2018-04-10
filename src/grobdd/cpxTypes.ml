(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

type elem =
	| S (* significant variable *)
	| P (* useless variable *)
	| X of (bool * int) (* if x then (i [2] XOR shift XOR neg) else ... *)

type block = {
	neg		: bool;
	shift	: bool;
	sub		: elem list;
}

type block2 = {
	negX	: bool;
	negY	: bool;
(* TODO the shiftX component could be factorised, think about it in next version *)
	shiftX	: bool;
	shiftY  : bool;
	subXY	: (elem * elem) list;
}

type edge_state = block
type node_state = block2
type node_and_state = block2
type node_xor_state = block2
type tacx_state = TacxTypes.tag * block2

type 'a edge = edge_state * (unit, 'a) Utils.gnode
type 'a node = node_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode
type 'a tacx = tacx_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode
