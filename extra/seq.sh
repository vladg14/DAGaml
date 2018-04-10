dagaml=~/DAGaml
(cd $dagaml; mkdir workdir; cd workdir; mkdir satlib-con; cd satlib-con; mkdir uf100-430 uf200-860 uf50-218 uuf150-645 uuf250-1065 uf125-538 uf20-91 uf75-325 uuf175-753 uuf50-218 uf150-645 uf225-960 uuf100-430 uuf200-860 uuf75-325 uf175-753 uf250-1065 uuf125-538 uuf225-960) &> /dev/null
extra=$dagaml/extra
bin=$dagaml/bin
select="python $extra/select-lines.py"
conv="$bin/test_conv_ext.native"
echo "" >> logs.fof
for src in $@
do
	dst=$(python $extra/extract_name.py $src)
	echo $dst
	echo "" > $dst.log
	(time $bin/test_icp.native $src $dst.v) &>> $dst.log
	echo "ICP generated"
	#time /home/micronova/abc-dir/abc -c "read out.v; strash; print_stats; dc2; dc2; dc2; dc2; dc2; print_stats; write out.v.v"
	(time $conv $dst .v $dst .cnf) &>> $dst.log
	echo "CNF generated"
	(time minisat $dst.cnf $dst.sat-log &> $dst.sat) &>> $dst.log
	echo "CNF solved"
	(time $bin/test_icp_post.native $src $dst.sat-log) &>> $dst.log
	echo "SOL post-processed"
	grep SAT $dst.log
done
