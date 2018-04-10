open ConvTypes

let conv_exti_to_tacx_cpA exti file args = match exti with
	| Cnf     -> ConvExtiCnfToIntTacxACp.conv_file_to_stree file args
	| Verilog -> ConvExtiVerilogToIntTacxACp.conv_file_to_stree file args
	| Pla     -> ConvExtiPlaToIntTacxACp.conv_file_to_stree file args

let conv_tacx_cpA_to_intA tacx_pure modele tacx_cpA args =
	match tacx_pure with
	| Tacx -> ConvIntTacxACpToIntTacxA_.conv_stree_stree modele tacx_cpA args
	| Pure -> ConvIntTacxACpToIntPureA_.conv_stree_stree modele tacx_cpA args

let conv_tacx_cpB_to_intB tacx_pure modele tacx_cpB args =
	match tacx_pure with
	| Tacx -> ConvIntTacxBCpToIntTacxB_.conv_stree_stree modele tacx_cpB args
	| Pure -> ConvIntTacxBCpToIntPureB_.conv_stree_stree modele tacx_cpB args

let conv_tacx_cpA_to_int (tacx_pure, modele, version) stree_cpA args =
	match version with
	| VA -> conv_tacx_cpA_to_intA tacx_pure modele stree_cpA args
	| VB ->
	(
		let stree_cpB = ConvIntTacxACpToIntTacxBCp.conv_stree_stree stree_cpA args in
		conv_tacx_cpB_to_intB tacx_pure modele stree_cpB args
	)

let conv tagA fileA tagB fileB args = match tagA, tagB with
| ExtI Verilog, ExtI Verilog ->
(
	let mexpr = StrLoadVerilog.load_file fileA in
	StrDumpVerilog.dump_file fileB mexpr;
)
| ExtI Verilog, ExtI Cnf ->
(
	let mexpr = StrLoadVerilog.load_file fileA in
	let cnf = ConvIntMExprToExtiCnf.conv_mexpr_to_cnf mexpr args in
	StrDumpCnf.dump_file fileB cnf;
)
| ExtI Cnf, ExtI Verilog ->
(
	let cnf = StrLoadCnf.load_file fileA in
	let mexpr = ConvExtiCnfToIntMExpr.conv_cnf_to_mexpr cnf args in
	StrDumpVerilog.dump_file fileB mexpr;
)
| ExtO _, _ -> failwith "[conv/conv] tagA is output_tag"
| _, ExtI _ -> failwith "[conv/conv] tagB is input_tag"
| Ext (), _
| _, Ext () -> failwith "[conv/conv] extern_tag is empty"
| ExtI exti, Int int ->
(
	let stree_cpA = conv_exti_to_tacx_cpA exti fileA args in
	let stree_int = conv_tacx_cpA_to_int int stree_cpA args in
	STree.dumpfile [stree_int] fileB
)
| Int(Tacx, Cp, VA), Int int ->
(
	let stree_cpA = STree.loadfile fileA |> List.hd in
	let stree_int = conv_tacx_cpA_to_int int stree_cpA args in
	STree.dumpfile [stree_int] fileB
)
| Int(Tacx, Cp, VB), Int (tacx_pure, modele, VB) ->
(
	let stree_cpB = STree.loadfile fileA |> List.hd in
	let stree_int = conv_tacx_cpB_to_intB tacx_pure modele stree_cpB args in
	STree.dumpfile [stree_int] fileB
)
| Int(Tacx, modele, VB), Int(Pure, modele', VB) when modele = modele' ->
(
	let stree_tacx = STree.loadfile fileA |> List.hd in
	let stree_pure = ConvIntTacxBXToPureBX.conv_stree_stree modele stree_tacx args in
	STree.dumpfile [stree_pure] fileB
)
| Int(Pure, Cp, VB), Int(Pure, Cpx, VB) ->
(
	let stree_cpB = STree.loadfile fileA |> List.hd in
	let stree_int = ConvIntPureBCpToIntPureBCpx.conv_stree_stree stree_cpB args in
	STree.dumpfile [stree_int] fileB
)
| Int(Tacx, modele, VA), ExtO Dot ->
(
	let stree = STree.loadfile fileA |> List.hd in
	ConvIntTacxA_ToExtoDot.conv_stree_file modele stree fileB args
)
| Int(Tacx, modele, VB), ExtO Dot ->
(
	let stree = STree.loadfile fileA |> List.hd in
	ConvIntTacxB_ToExtODot.conv_stree_file modele stree fileB args
)
| Int(Pure, modele, VB), ExtO Dot ->
(
	let stree = STree.loadfile fileA |> List.hd in
	ConvIntPureB_ToExtODot.conv_stree_file modele stree fileB args
)
| Int(Tacx, Cpx, VB), ExtO Check ->
(
	let stree = STree.loadfile fileA |> List.hd in
	ConvIntTacxBCpxToExtOCheck.conv_stree_file stree fileB args
)
| Int(Tacx, modele, VA), ExtO Stats ->
(
	let stree = STree.loadfile fileA |> List.hd in
	ConvIntTacxA_ToExtStats.conv_stree_file modele stree fileB args
)
| Int(Pure, modele, VA), ExtO Stats ->
(
	let stree = STree.loadfile fileA |> List.hd in
	ConvIntPureA_ToExtStats.conv_stree_file modele stree fileB args
)
| Int(Pure, modele, VA), ExtO Dot ->
(
	let stree = STree.loadfile fileA |> List.hd in
	ConvIntPureA_ToExtODot.conv_stree_file modele stree fileB args
)
| Int(Pure, modele, VB), ExtO CntSat ->
(
	let stree = STree.loadfile fileA |> List.hd in
	ConvIntPureB_ToExtOCntSat.conv_stree_file modele stree fileB args
)
| _ -> failwith "[conv/conv] WIP conv"
