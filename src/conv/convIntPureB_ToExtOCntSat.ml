open ConvTypes
let conv_stree_file modele stree fileB args =
	let channel = open_out fileB in
	let output = output_string channel in
	let cntsat = match modele with
	| Bryant -> failwith "WIP export .bryant.B.pure to .cntsat"
	| Zdd    -> failwith "WIP export .zdd.B.pure to .cntsat"
	| Cp     ->
	(
		let pure, edges = CpB.GroBdd.stree_load stree in
		CpB.GroBdd.cntsat pure edges |> snd
	)
	| Nni    ->
	(
		let pure, edges = NniB.GroBdd.stree_load stree in
		NniB.GroBdd.cntsat pure edges |> snd
	)
	| Cpx    ->
	(
		let pure, edges = Cpx2B.GroBdd.stree_load stree in
		Cpx2B.GroBdd.cntsat pure edges |> snd
	)
	in
	output (StrDump.list BigInt.to_string cntsat);
	output "\n";
	close_out channel
