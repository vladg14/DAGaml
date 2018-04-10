open Extra

let system_call s : unit =
	ignore (Unix.system s)

let parse inf sup target : (bool array * bool array * int) option =
	let file = open_in target in
	let line = input_line file in
	if line.[0] = 'U' && line.[1] = 'N'
	then None
	else (
		let t =
				 input_line file
			|> StrUtil.split ' '
			||> (fun w -> not(w.[0] = '-'))
			|> Array.of_list in
		close_in file;
		let ts = Array.sub t 0 (inf-1)
		and tb = Array.sub t (inf-1) (sup-inf+1) in
		let nb = Tools.int_of_bin (Array.to_list tb) in
		Some(ts, tb, nb)
	)

let extend min sol =
	let len_min = Array.length min
	and len_sol = Array.length sol in 
	assert(len_min <= len_sol);
	assert(min = Array.sub sol 0 len_min);
	let rec aux idx cnt =
		if idx < len_sol && sol.(idx) = false
		then (aux (idx+1) (cnt+1))
		else cnt
	in
	let cnt = aux len_min 0 in
	(cnt, Array.append min (Array.make cnt false))

let string_of_barray a = a
	|> Array.to_list
	||> StrUtil.char_of_bool
	|> StrUtil.implode

let multiset_false file start len =
	assert(start-len+1>=0);
	for idx = start downto (start-len+1)
	do
		system_call ("echo \"-"^(string_of_int idx)^" 0\" >> "^file)
	done;
	()

let main_aux idx min_ sol min old_f new_f log_f sup inf time_init rmin : bool array * bool array =
	let rec aux idx min_ sol min rmin : bool array * bool array =
		if idx < inf then (sol, min) else (
			print_string ("time = "^(string_of_float (Sys.time()-.time_init))); print_newline();
			print_string ("Min["^(string_of_int idx)^"] = "^(string_of_barray min_)); print_newline();
			system_call
				("cp "^old_f^" "^new_f^";"^
				 "echo \"-"^(string_of_int idx)^" 0\" >> "^new_f^";"^
				 "minisat "^new_f^" "^log_f^" &> /dev/null");
			match parse inf sup log_f with
			| None -> (
				let j, min_ = extend (Array.append min_ [|true |]) rmin in
				system_call ("echo \""^(string_of_int idx)^" 0\" >> "^old_f);
				multiset_false old_f (idx-1) j;
				aux (idx-j-1) min_ sol min rmin
			)
			| Some (sol, min, value) -> (
				let rmin = MyArray.rev min in
				print_string ("-"^(string_of_barray rmin)); print_newline();
				let j, min_ = extend (Array.append min_ [|false|]) rmin in
				system_call ("mv "^new_f^" "^old_f);
				multiset_false old_f (idx-1) j;
				aux (idx-j-1) min_ sol min rmin
			)
		)
	in aux idx min_ sol min rmin

let main inf sup cnf =
	let time_init = Sys.time () in
	assert(inf <= sup);
	let old_f = cnf^".old.cnf"
	and new_f = cnf^".new.cnf"
	and log_f = cnf^".sat-log"
	and out_f = cnf^".out" in
	system_call ("cp "^cnf^" "^old_f);
	system_call ("time minisat "^old_f^" "^log_f^" &> /dev/null");
	match parse inf sup log_f with
	| None -> (
		let file = open_out out_f in
		output_string file "UNSAT";
		close_out file;
		()
	)
	| Some (sol, min, value) -> (
		let rmin = MyArray.rev min in
		print_string ("-"^(string_of_barray rmin)); print_newline();
		let j, min_ = extend [||] rmin in
		multiset_false old_f sup j;
		let sol, min = main_aux (sup-j) min_ sol min old_f new_f log_f sup inf time_init rmin in
		let file = open_out out_f in
		output_string file ("SAT\n"^(string_of_barray sol)^"\n"^(string_of_barray min)^"\n");
		close_out file;
		()
	)

let _ = main
	(int_of_string Sys.argv.(1))
	(int_of_string Sys.argv.(2))
	Sys.argv.(3)
	
