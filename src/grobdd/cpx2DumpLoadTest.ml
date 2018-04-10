(* All Right Reserved

   Copyright (c) 2017 Joan Thibault
*)

open IterExtra
open Cpx2Types
open Cpx2TypesTest
open Cpx2Utils
open Cpx2DumpLoad



let n = int_of_string(Sys.argv.(1));;

let _ = 

	let test_case0 = {
		neg = true;
		arity = 3;
		block = SPX(
			true,
			{hasS = false; hasP = false; maxX = Some 2},
			[X(true, 0); X(true, 1); X(true, 2)]
		)
	} in
	print_string(dummydump_block test_case0); print_newline();
	ignore(bindump_block test_case0 (is_nil test_case0) []);
	let test_case1 = {neg = true; arity = 4; block = SPX(true, {hasS = false; hasP = false; maxX = Some (3)}, [X(true, 0); X(true, 1); X(true, 2); X(true, 3)])} in
	print_string(dummydump_block test_case1); print_newline();
	let nil = is_nil test_case1 in
	let stream = bindump_block test_case1 nil [] in
	StrUtil.print_stream stream; print_newline();
	ignore(binload_block nil stream);

	(*print_string "TEST 3.0.2.0 : check log2"; print_newline();
	Iter.iter (fun x ->
		print_int x;
		print_string "\t";
		print_int (log2 x);
		print_newline()) (Iter.range 1 16);*)
	print_string "TEST 3.0.2 : binDump/Load checked block"; print_newline();
	Iter.iter (fun block ->
		let nil = is_nil block in
		let stream =
			try				bindump_block block nil []
			with _ -> print_string(dummydump_block block); print_newline(); assert(false)
		in
		let block', stream' =
			try				binload_block nil stream
			with _ ->
		(
			print_string(dummydump_block block); print_newline();
			StrUtil.print_stream stream; print_newline();
			assert(false)
		)
		in
		assert(stream' = []);
		assert(block = block')) (gen_block_checked n);

