(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

type invar = bool * (Bitv.t option)

type uniq_elem =
	| S of bool
	| P of invar

type pair_elem =
	| SS of bool
	| SP of invar
	| PS of invar
	| PP of invar * invar

type uniq = uniq_elem list
type pair = pair_elem list

type edge_state = bool * uniq
type node_state = bool * pair
type node_and_state = bool * bool * pair
type node_xor_state =               pair
type tacx_state = TacxTypes.ttag * pair

type node_cstate = Bitv.t
type edge_cstate = Bitv.t
type tacx_cstate = Bitv.t

type 'a edge = edge_state * (unit, 'a) Utils.gnode
type 'a node = node_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode
type 'a tacx = tacx_state * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode


type 'a cedge = edge_cstate * (unit, 'a) Utils.gnode
type 'a cnode = node_cstate * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode
type 'a ctacx = tacx_cstate * (unit, 'a) Utils.gnode * (unit, 'a) Utils.gnode

type iinvar = bool * (bool list option)

type iuniq_elem =
	| IS of bool
	| IP of iinvar

type iuniq = iuniq_elem list

type ipair_elem =
	| ISS of bool
	| IPS of iinvar
	| ISP of iinvar
	| IPP of iinvar * iinvar

type ipair = ipair_elem list

let count_s = MyList.count (function S _ -> true | P _ -> false)
let count_p = MyList.count (function S _ -> false | P _ -> true)

let count_is = MyList.count (function IS _ -> true | IP _ -> false)
let count_ip = MyList.count (function IS _ -> false | IP _ -> true)

let iinvar_of_invar (b, opi) = (b, (match opi with
	| None -> None
	| Some i -> Some(Bitv.L.to_bool_list i)))

let invar_of_iinvar (b, opi) = (b, (match opi with
	| None -> None
	| Some i -> Some(Bitv.L.of_bool_list i)))

let xiuniq_of_xuniq = List.map (function
	| S b -> IS b
	| P i -> IP (iinvar_of_invar i))

let xuniq_of_xiuniq = List.map (function
	| IS b -> S b
	| IP i -> P (invar_of_iinvar i))

let ipair_of_pair = List.map (function
	| SS b -> ISS b
	| PS i -> IPS (iinvar_of_invar i)
	| SP i -> ISP (iinvar_of_invar i)
	| PP (ix, iy) -> IPP (iinvar_of_invar ix, iinvar_of_invar iy))

let pair_of_ipair = List.map (function
	| ISS b -> SS b
	| IPS i -> PS (invar_of_iinvar i)
	| ISP i -> SP (invar_of_iinvar i)
	| IPP (ix, iy) -> PP (invar_of_iinvar ix, invar_of_iinvar iy))
