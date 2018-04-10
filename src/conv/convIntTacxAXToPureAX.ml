let conv_stree_stree modele stree args = match modelel with
| Bryant	-> failwith "no conversion available from .cp.tacx to .bryant.tacx"
| Zdd			-> failwith "no conversion available from .cp.tacx to .zdd.tacx"
| Cp      -> ConvIntTacxACpToIntPureACp.conv_stree_stree stree args
| Nni     -> ConvIntTacxANniToIntPureANni.conv_stree_stree stree args
| Cpx			-> ConvIntTacxACpxToIntPureACpx.conv_stree_stree stree args
