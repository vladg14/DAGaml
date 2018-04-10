open ConvTypes
let conv_stree_stree modele tacx_cp args = match modele with
| Bryant	->
(
	let pure_cp = ConvIntTacxACpToIntPureACp.conv_stree_stree tacx_cp args in
	ConvIntPureACpToIntPureABryant.conv_stree_stree pure_cp args
)
| Zdd			->
(
	let pure_cp = ConvIntTacxACpToIntPureACp.conv_stree_stree tacx_cp args in
	ConvIntPureACpToIntPureAZdd.conv_stree_stree pure_cp args
)
| Cp      ->
(
	ConvIntTacxACpToIntPureACp.conv_stree_stree tacx_cp args
)
| Nni     ->
(
	let tacx_nni = ConvIntTacxACpToIntTacxA_.conv_cp_to_nni tacx_cp args in
	ConvIntTacxANniToIntPureANni.conv_stree_stree tacx_nni args
)
| Cpx     ->
(
	let tacx_cpx = ConvIntTacxACpToIntTacxA_.conv_cp_to_cpx tacx_cp args in
	ConvIntTacxACpxToIntPureACpx.conv_stree_stree tacx_cpx args
)
