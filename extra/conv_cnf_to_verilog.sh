src="DAGaml-benchmarks/satlib/$1/"
dst="DAGaml-benchmarks/satlib-v/$1/"
dagaml=~/DAGaml
(cd $dagaml; mkdir DAGaml-benchmarks; cd DAGaml-benchmarks; mkdir satlib-v; cd satlib-v; mkdir uf100-430 uf200-860 uf50-218 uuf150-645 uuf250-1065 uf125-538 uf20-91 uf75-325 uuf175-753 uuf50-218 uf150-645 uf225-960 uuf100-430 uuf200-860 uuf75-325 uf175-753 uf250-1065 uuf125-538 uuf225-960) &> /dev/null
extra=$dagaml/extra
bin=$dagaml/bin
conv="$bin/test_conv_ext.native"
for file in $(cd $src; ls *.cnf | sed  "s/\.cnf$//")
do
	echo $file
	$conv $src/$file .cnf $dst/$file .v
done


