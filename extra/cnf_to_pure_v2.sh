dagaml=~/DAGaml
(cd $dagaml; mkdir workdir; cd workdir; mkdir satlib; cd satlib; mkdir uf100-430 uf200-860 uf50-218 uuf150-645 uuf250-1065 uf125-538 uf20-91 uf75-325 uuf175-753 uuf50-218 uf150-645 uf225-960 uuf100-430 uuf200-860 uuf75-325 uf175-753 uf250-1065 uuf125-538 uuf225-960) &> /dev/null
extra=$dagaml/extra
bin=$dagaml/bin
select="python $extra/select-lines.py"
conv="$bin/test_conv_ext.native"
echo "" >> logs.fof
for src in $@
do
	dst=$(python $extra/extract_name.py $src)
	echo "$dst"
	echo " -         cnf to   cp.A.tacx (v3)"
	(time $bin/test_upgrade_benchmark_cnf_to_tacx_cp_v3.native $src $dst.3.cp.A.tacx) &> $dst.3.cnf-to-cpA.log
	echo " -   cp.A.tacx to  cpx.B.pure (v3)"
	(time $conv $dst.3 .cp.A.tacx  $dst.3 .cpx.B.pure) &> $dst.3.cpA-to-cpxB.log
	echo " - export cpx.B.pure to .cntsat (v3)"
	(time $conv $dst.3 .cpx.B.pure $dst.3 .cntsat) &> $dst.cntsat-v3.log
	echo " - export cp.A.tacx to .stats (v3)"
	(time $conv $dst.3 .cp.A.tacx  $dst.3.tacx .stats) &> $dst.tacx-stats-v3.log
	echo " - export cpx.B.pure to .stats (v3)"
	(time $conv $dst.3 .cpx.B.pure $dst.3.pure .stats) &> $dst.pure-stats-v3.log
	log=$dst.stats.log
	echo $dst > $log
	echo $log > logs.fof
	echo "#force_cst = 0" >> $log
	cat $dst.3.cntsat >> $log
	python $extra/pick.py --strip " \t" : """$(grep m $dst.3.cpA-to-cpxB.log | grep s)""" >> $log
	python $extra/pick.py --strip "():# \"" 4 """$(cat $dst.3.tacx.stats)""" >> $log
	for i in $(seq 1 10 100)
	do
		echo " -         cnf to   cp.A.tacx (v4)"
		(time $bin/test_upgrade_benchmark_cnf_to_tacx_cp_v4.native $src $dst.4.$i.cp.A.tacx $i) &> $dst.4.$i.cnf-to-cpA.log
		echo " -   cp.A.tacx to  cpx.B.pure (v4)"
		(time $conv $dst.4.$i .cp.A.tacx  $dst.4.$i .cpx.B.pure) &> $dst.4.$i.cpA-to-cpxB.log
		echo " - export cpx.B.pure to .cntsat (v4)"
		(time $conv $dst.4.$i .cpx.B.pure $dst.4.$i .cntsat) &> $dst.$i.cntsat-v4.log
		echo " - export cp.A.tacx to .stats (v4)"
		(time $conv $dst.4.$i .cp.A.tacx  $dst.4.$i.tacx .stats) &> $dst.$i.tacx-stats-v4.log
		echo " - export cpx.B.pure to .stats (v4)"
		(time $conv $dst.4.$i .cpx.B.pure $dst.4.$i.pure .stats) &> $dst.$i.pure-stats-v4.log
		echo "#force_cst = " $i >> $log
		cat $dst.4.$i.cntsat >> $log
		python $extra/pick.py --strip " \t" : """$(grep m $dst.4.$i.cpA-to-cpxB.log | grep s)""" >> $log
		python $extra/pick.py --strip "():# \"" 4 """$(cat $dst.4.$i.tacx.stats)""" >> $log
		#python $extra/pick.py --strip "():# \"" : """$(cat $dst.4.$i.pure.stats)""" >> $log
	done
done
python extra/select-lines.py "[i for i in range(1, 67) if i%6 != 2]" $(cat logs.fof)
