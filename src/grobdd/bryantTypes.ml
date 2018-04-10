(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

type uniq = int * int
type pair = int * int * int

type edge_state = bool * uniq
type node_state = bool * pair
type node_and_state = bool * bool * pair
type node_xor_state =               pair
type tacx_state = TacxTypes.ttag * pair

type 'a edge = edge_state * (unit, 'a) Utils.gnode
type 'a node = node_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode
type 'a tacx = tacx_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode
