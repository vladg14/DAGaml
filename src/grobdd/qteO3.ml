open O3Extra
open BinO3
open Qte

let dump_qte_elem dump_trans elem stream = match elem with
	| Some(Q quant) -> false::false::(fst QuantO3.o3s_quant    quant stream)
	| Some(T trans) -> false::true ::(dump_trans               trans stream)
	| Some(E peval) -> true ::false::(BinDump.bool_option_list peval stream)
	| None          -> true ::true ::stream

let load_qte_elem load_trans = function
	| false::false::stream ->
	(
		let quant, stream = snd QuantO3.o3s_quant stream in
		(Some(Q quant), stream)
	)
	| false::true ::stream ->
	(
		let trans, stream = load_trans stream in
		(Some(T trans), stream)
	)
	| true ::false::stream ->
	( 
		let peval, stream = BinLoad.bool_option_list stream in
		(Some(E peval), stream)
	)
	| true ::true ::stream ->
		(None, stream)
	| _ -> assert false

let o3s_qte_elem (dump_trans, load_trans) =
	(dump_qte_elem dump_trans, load_qte_elem load_trans)

let o3_qte : ('i qte, int * ('i qte_elem list)) O3.o3 =
(
	(fun qte -> (qte.arity, qte.block)),
	(fun (arity, block) -> {arity; block})
)

let o3s_qte (o3s_trans : 'trans BinO3.o3s) =
	let o3s_block : 'trans qte_elem list BinO3.o3s = none_list (o3s_qte_elem o3s_trans) in
	o3_qte +>> O3.pair (int, o3s_block)

let strdump_qte_elem strdump_trans = function
| Q quant -> "Q("^(QuantO3.strdump_quant quant)^")"
| T trans -> "T("^(strdump_trans trans)^")"
| E peval -> "E("^(GUtils.strdump_peval peval)^")"

let strdump_qte strdump_trans qte =
	StrDump.("{arity = "^(int qte.arity)^"; block = "^(list (strdump_qte_elem strdump_trans) qte.block)^"}")

let default_o3s_node o3s_trans =
	O3.trio (TacxTypes.o3s_tag, o3s_qte o3s_trans, o3s_qte o3s_trans)


