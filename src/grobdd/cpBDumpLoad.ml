(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open CpTypes
open Extra

let strdump_uniq_elem = function
	| P -> "U"
	| S -> "S"

let strload_uniq_elem = function
	| 'U' -> P
	| 'S' -> S
	| _   -> assert false

let strdump_edge (neg, sub) =
	String.concat "" ((if neg then "-" else "+")::(List.map CpGops.strdump_uniq_elem sub))

let bindump_uniq_elem elem stream = match elem with
	| P -> false::stream
	| S -> true ::stream

let binload_uniq_elem = function
	| false::stream -> P, stream
	| true ::stream -> S, stream
	| _ -> assert false


let strdump_uniq = List.map strdump_uniq_elem
let strload_uniq = List.map strload_uniq_elem

let bindump_uniq = BinDump.list bindump_uniq_elem
let binload_uniq = BinLoad.list binload_uniq_elem

let bindump_pair_elem elem stream = match elem with
	| Some SP -> false::false::stream
	| Some PS -> false::true ::stream
	| Some SS -> true ::false::stream
	| None    -> true ::true ::stream

let binload_pair_elem = function
	| false::false::stream -> Some SP, stream
	| false::true ::stream -> Some PS, stream
	| true ::false::stream -> Some SS, stream
	| true ::true ::stream -> None   , stream
	| _ -> assert false

let merge_pair uniqX uniqY = List.map2 (fun x y -> match x, y with
	| P, P -> assert false
	| S, P -> SP
	| P, S -> PS
	| S, S -> SS) uniqX uniqY

let split_pair pair = pair |> List.map (function
	| SS -> S, S
	| SP -> S, P
	| PS -> P, S) |> List.split

let bindump_pair : CpTypes.pair BinUtils.dump = BinDump.none_list bindump_pair_elem
let binload_pair : CpTypes.pair BinUtils.load = BinLoad.none_list binload_pair_elem

let bindump_edge : CpTypes.edge_state BinUtils.dump =
	BinDump.pair BinDump.bool bindump_uniq
let binload_edge : CpTypes.edge_state BinUtils.load =
	BinLoad.pair BinLoad.bool binload_uniq

(* version 1 *)

let bindump_node = BinDump.(trio unit bindump_edge bindump_edge)
let binload_node = BinLoad.(trio unit binload_edge binload_edge)

(* version 2 *)
let bindump_node ((), (bX, lX), (bY, lY)) =
	let lXY = merge_pair lX lY in
	BinDump.(trio bool bool bindump_pair) (bX, bY, lXY)
let binload_node stream =
	let (bX, bY, lXY), stream = BinLoad.(trio bool bool binload_pair) stream in
	let lX, lY = split_pair lXY in
	((), (bX, lX), (bY, lY)), stream

let bindump_tacx = BinDump.trio TacxTypes.bindump_tag bindump_edge bindump_edge
let binload_tacx = BinLoad.trio TacxTypes.binload_tag binload_edge binload_edge
