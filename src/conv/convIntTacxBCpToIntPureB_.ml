open ConvTypes
let conv_stree_stree modele tacx_cp args = match modele with
| Bryant	->
(
	let pure_cp = ConvIntTacxBCpToIntPureBCp.conv_stree_stree tacx_cp args in
	ConvIntPureBCpToIntPureBBryant.conv_stree_stree pure_cp args
)
| Zdd			->
(
	let pure_cp = ConvIntTacxBCpToIntPureBCp.conv_stree_stree tacx_cp args in
	ConvIntPureBCpToIntPureBZdd.conv_stree_stree pure_cp args
)
| Cp      ->
(
	ConvIntTacxBCpToIntPureBCp.conv_stree_stree tacx_cp args
)
| Nni     ->
(
	let tacx_nni = ConvIntTacxBCpToIntTacxB_.conv_cp_to_nni tacx_cp args in
	ConvIntTacxBNniToIntPureBNni.conv_stree_stree tacx_nni args
)
| Cpx     ->
(
	let tacx_cpx = ConvIntTacxBCpToIntTacxB_.conv_cp_to_cpx tacx_cp args in
	ConvIntTacxBCpxToIntPureBCpx.conv_stree_stree tacx_cpx args
)
