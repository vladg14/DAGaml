type 'trans qte_elem =
| Q of  Quant.quant
| T of 'trans
| E of  GUtils.peval

type 'trans qte = {arity : int; block : 'trans qte_elem list}

let solve_node ((tag, (qte0, next0), (qte1, next1)) as node) =
	assert(qte0.arity = qte1.arity);
	let arity = qte0.arity + TacxTypes.(match tag with Cons -> 1 | _ -> 0) in
	({arity; block = []}, Utils.MNode node)

let compose qteC (qte, next) = ({arity = qteC.arity; block = qteC.block@qte.block}, next)
