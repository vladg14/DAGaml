open ConvTypes

let str_ext = Sys.argv.(1);;
print_string str_ext;;
print_newline();;
let ext = ConvTypesLoad.file_ext str_ext;;
let str_ext' = ConvTypesDump.file_ext ext;;
print_string str_ext';;
print_newline();;
exit 0;;
