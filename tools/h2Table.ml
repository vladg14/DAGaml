type 'a t = {
	access : ('a, int) Hashtbl.t;
	revers : (int, 'a) Hashtbl.t;
	mutable index : int;
};;

let create n i =
	{access = Hashtbl.create n; revers = Hashtbl.create n; index = i}
;;

let memA tbl item = Hashtbl.mem tbl.access item;;
let memI tbl indx = Hashtbl.mem tbl.revers indx;;

let push tbl item =
	try
		Hashtbl.find tbl.access item
	with Not_found ->(
		let index = tbl.index in
		tbl.index<-tbl.index+1;
		if tbl.index < 0 then failwith "H2Table - negative index";
		Hashtbl.add tbl.access item index;
		Hashtbl.add tbl.revers index item;
		index)
;;

let pull tbl indx =
	if Hashtbl.mem tbl.revers indx
	then Hashtbl.find tbl.revers indx
	else failwith "H2Table - undefined index";;

let length tbl =
	Hashtbl.length (tbl.access)
;;

let iter tbl fonc =
	Hashtbl.iter fonc (tbl.access);
;;
