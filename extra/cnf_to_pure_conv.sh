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
	echo " -         cnf to   cpx.B.pure"
	(time $bin/test_conv.native $src .cnf $dst.cpx.B.pure .cpx.B.pure) &> $dst.cnf-to-cpA.log
	#log=$dst.stats.log
	#echo $dst > $log
	#echo $log >> logs.fof
	#cat $dst.3.cntsat >> $log
	#cat $dst.4.cntsat >> $log
	#python $extra/pick.py --strip " \t" : """$(grep m $dst.3.cpA-to-cpxB.log | grep s)""" >> $log
	#python $extra/pick.py --strip " \t" : """$(grep m $dst.4.cpA-to-cpxB.log | grep s)""" >> $log
	#python $extra/pick.py --strip "():# \"" 4 """$(cat $dst.3.tacx.stats)""" >> $log
	#python $extra/pick.py --strip "():# \"" 4 """$(cat $dst.4.tacx.stats)""" >> $log
	#python $extra/pick.py --strip "():# \"" : """$(cat $dst.3.pure.stats)""" >> $log
	#python $extra/pick.py --strip "():# \"" : """$(cat $dst.4.pure.stats)""" >> $log
done
python extra/select-lines.py "list(range(1, 12))" $(cat logs.fof)
