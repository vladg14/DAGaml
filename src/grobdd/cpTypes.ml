(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

type uniq_elem = P | S
type pair_elem = SS | SP | PS

type uniq = uniq_elem list
type pair = pair_elem list

type edge_state = bool * uniq
type node_state = bool * pair
type node_and_state = bool * bool * pair
type node_xor_state =               pair
type tacx_state = TacxTypes.ttag * pair

type 'a edge = edge_state * (unit, 'a) Utils.gnode
type 'a node = unit           * 'a edge * 'a edge
type 'a tacx = TacxTypes.ttag * 'a edge * 'a edge
