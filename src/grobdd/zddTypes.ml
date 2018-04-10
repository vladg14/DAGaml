(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

type uniq = int * int
type pair = int * int * int

type edge_state = uniq
type node_state = pair

type node_cstate = Bitv.t
type edge_cstate = Bitv.t

type 'a edge = edge_state * (unit, 'a) Utils.gnode
type 'a node = node_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode


type 'a cedge = edge_cstate * (unit, 'a) Utils.gnode
type 'a cnode = node_cstate * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode
