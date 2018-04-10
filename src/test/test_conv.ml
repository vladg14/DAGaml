let fileA = Sys.argv.(1)
and extA  = Sys.argv.(2) |> ConvTypesLoad.file_ext
and fileB = Sys.argv.(3)
and extB  = Sys.argv.(4) |> ConvTypesLoad.file_ext;;

Conv.conv extA fileA extB fileB [ConvArgsTypes.print_stats];;
print_newline();;
exit 0;;
