dagaml=~/DAGaml
(cd $dagaml; mkdir workdir; cd workdir; mkdir satlib; cd satlib; mkdir uf100-430 uf200-860 uf50-218 uuf150-645 uuf250-1065 uf125-538 uf20-91 uf75-325 uuf175-753 uuf50-218 uf150-645 uf225-960 uuf100-430 uuf200-860 uuf75-325 uf175-753 uf250-1065 uuf125-538 uuf225-960) &> /dev/null
extra=$dagaml/extra
bin=$dagaml/bin
select="python $extra/select-lines.py"
conv="$bin/test_conv_ext.native"
for src in $@
do
	dst=$(python $extra/extract_name.py $src)
	echo "$dst"
	echo " -         cnf to   cp.A.tacx"
	(time $bin/test_conv.native $src .cnf $dst.cp.A.tacx .cp.A.tacx) &> $dst.cnf-to-cpA.log
	echo " -   cp.A.tacx to  cpx.B.pure"
	(time $conv $dst .cp.A.tacx $dst .cpx.B.pure) &> $dst.cpA-to-cpxB.log
	echo " -   cp.A.tacx to   cp.B.pure"
	(time $conv $dst .cp.A.tacx $dst .cp.B.pure ) &> $dst.cpA-to-cpB.log
	echo " - export cpx.B.pure to .cntsat"
	(time $conv $dst .cpx.B.pure $dst.cpx.B.pure .cntsat) &> $dst.cntsat-cpxB.log
	echo " - export cp.B.pure  to .cntsat"
	(time $conv $dst .cp.B.pure  $dst.cp.B.pure  .cntsat) &> $dst.cntsat-cpB.log
	log=$dst.stats.log
	echo $dst > $log
	cat $dst.cp.B.pure.cntsat >> $log
	cat $dst.cpx.B.pure.cntsat >> $log
done
