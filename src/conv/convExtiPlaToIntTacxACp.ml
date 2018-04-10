let conv_file_to_stree file_in args =
	let module_expr = StrLoadPla.load (Stream.of_channel (open_in file_in))in
	let my_tacx     = IoUtils.module_tacx_of_module_expr module_expr in
	IoUtils.module_tacx_stree_dump my_tacx

