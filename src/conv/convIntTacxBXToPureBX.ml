open ConvTypes
let conv_stree_stree modele stree args = match modele with
| Bryant	-> failwith "no conversion available from .cp.tacx to .bryant.tacx"
| Zdd			-> failwith "no conversion available from .cp.tacx to .zdd.tacx"
| Cp      -> ConvIntTacxBCpToIntPureBCp.conv_stree_stree stree args
| Nni     -> ConvIntTacxBNniToIntPureBNni.conv_stree_stree stree args
| Cpx			-> ConvIntTacxBCpxToIntPureBCpx.conv_stree_stree stree args
