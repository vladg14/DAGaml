open Cpx2BTypes
open StrDump

let elem_spx = function
	| S -> "S"
	| P -> "P"
	| X(b, i) -> ("X("^(bool b)^", "^(int i)^")")

let spx_tag tag' =
	"{hasS = "^(bool tag'.hasS)^"; hasP = "^(bool tag'.hasP)^"; maxX = "^(option int tag'.maxX)^"}"
let block_spx = trio bool spx_tag (list elem_spx)

let block_choice = function
	| C0 -> "C0"
	| Id x' -> "Id "^(int x')
	| SPX block_spx' -> "SPX "^(block_spx block_spx')

let block block' =
	"{neg = "^(bool block'.neg)^"; arity = "^(int block'.arity)^"; block = "^(block_choice block'.block)^"}"

let next next' = UtilsDump.gnode (fun () -> "") (fun _ -> " _ ") next'
let edge edge' = pair block next edge'
let node node' = trio unit edge edge node'

let merge x' = UtilsDump.merge next node x'
let emerge x' = UtilsDump.emerge block merge x'

let merge3 x' = UtilsDump.merge3 next node x'
let emerge3 x' = UtilsDump.emerge3 block merge3 x'

let pnext next' = StrDump.(UtilsDump.gnode unit (pair GUtils.strdump_opeval ignore) next')
let pedge edge' = pair block pnext edge'
let pnode node' = trio unit pedge pedge node'

let pmerge x' = UtilsDump.merge pnext pnode x'
let pemerge x' = UtilsDump.emerge block pmerge x'

let pmerge3 x' = UtilsDump.merge3 pnext pnode x'
let pemerge3 x' = UtilsDump.emerge3 block pmerge3 x'
