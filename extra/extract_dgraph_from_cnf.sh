src="benchmarks/satlib/$1/"
dst="benchmarks/satlib-con/$1/"
for file in $(cd $src; ls *.cnf | sed  "s/\.cnf$//")
do
	echo $file
	./test_extract_dgraph_from_cnf.native $src/$file.cnf $dst/$file.con
done


