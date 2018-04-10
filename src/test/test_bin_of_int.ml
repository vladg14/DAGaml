for i = 0 to 256
do
	print_int i;
	print_string "\t-> ";
	Array.iter StrUtil.print_bool (Tools.bin_of_int i);
	print_newline();
done;
