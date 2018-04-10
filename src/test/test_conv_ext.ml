let n = (Array.length Sys.argv-1)/2-1;;
assert(n>=1);
assert(2*(n+1)+1 = Array.length Sys.argv);
(* start by checking validity of all extensions *)
for idx = 0 to n
do
	ConvTypesLoad.file_ext Sys.argv.(2*idx+2) |> ignore
done;;
for idx = 0 to n-1
do
	let k = 2*idx in 
	let fileA = Sys.argv.(k+1)
	and extA  = Sys.argv.(k+2)
	and fileB = Sys.argv.(k+3)
	and extB  = Sys.argv.(k+4) in

	let fileA = fileA^extA
	and tagA  = ConvTypesLoad.file_ext extA
	and fileB = fileB^extB
	and tagB  = ConvTypesLoad.file_ext extB in

	Conv.conv tagA fileA tagB fileB [];(*ConvArgsTypes.print_stats];;*)
	print_newline()
done;;
exit 0;;
