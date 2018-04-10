dagaml=~/DAGaml
(cd $dagaml; mkdir workdir; cd workdir; mkdir satlib-con; cd satlib-con; mkdir uf100-430 uf200-860 uf50-218 uuf150-645 uuf250-1065 uf125-538 uf20-91 uf75-325 uuf175-753 uuf50-218 uf150-645 uf225-960 uuf100-430 uuf200-860 uuf75-325 uf175-753 uf250-1065 uuf125-538 uuf225-960) &> /dev/null
extra=$dagaml/extra
bin=$dagaml/bin
select="python $extra/select-lines.py"
conv="$bin/test_conv_ext.native"
abc=~/abc-dir/abc
echo "" >> logs.fof
for src in $@
do
	dst=$(python $extra/extract_name.py $src)
	echo "$dst"
	echo "" > $dst.log
	echo "synthetizing ICP:CNF"
	(time $bin/test_icp.native $src $dst.cnf -cnf) &>> $dst.log
	echo "solve:optimize CNF"
	#(time python $extra/optimize.py 401 421 $dst.cnf) &>> $dst.log
	(time $bin/optimize.native 401 421 $dst.cnf) &>> $dst.log
	echo "done!"
done
