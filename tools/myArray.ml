let count f a =
	let cnt = ref 0 in
	Array.iteri (fun i x -> if f x then incr cnt) a;
	!cnt

let counti f a =
	let cnt = ref 0 in
	Array.iteri (fun i x -> if f i x then incr cnt) a;
	!cnt

let indexes f a = Array.of_list (MyList.indexes f (Array.to_list a))
