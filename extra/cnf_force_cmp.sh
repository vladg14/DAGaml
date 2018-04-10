dagaml=~/DAGaml
(cd $dagaml; mkdir workdir; cd workdir; mkdir satlib; cd satlib; mkdir uf100-430 uf200-860 uf50-218 uuf150-645 uuf250-1065 uf125-538 uf20-91 uf75-325 uuf175-753 uuf50-218 uf150-645 uf225-960 uuf100-430 uuf200-860 uuf75-325 uf175-753 uf250-1065 uuf125-538 uuf225-960) &> /dev/null
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
	echo "" > $dst.out
	echo "cnf       to robdd (bryant)"
	cp $src $dst.cnf
	(time $conv $dst .cnf $dst.default .cp.A.pure $dst.default .stats) &>> $dst.log
	python $extra/pick.py --strip "()[]:# \"" 4 """$(cat $dst.default.stats)""" >> $dst.out
	echo "cnf-force to robdd (bryant)"
  (time ($extra/force/cnf2hgraph.pl $src; \
				$extra/force/NetPlacer -c 6; \
				$extra/force/cnf_rename_vars.pl out.pl $src $dst.force.cnf)) &>> $dst.log
	(time $conv $dst.force .cnf $dst.force .cp.A.pure $dst.force .stats) &>> $dst.log
	python $extra/pick.py --strip "()[]:# \"" 4 """$(cat $dst.force.stats)""" >> $dst.out
	echo "cnf-wap   to robdd (bryant)"
	(time $bin/test_extract_dgraph_from_cnf.native $src $dst.wap.con) &>> $dst.log
	(time $bin/test_icp.native $dst.wap.con $dst.wap.cnf -cnf) &>> $dst.log
	#(time $abc -c "read $dst.wap.v; strash; print_stats; dc2; dc2; dc2; dc2; dc2; print_stats; write $dst.wap.opt.v") &>> $dst.log
	#(time $conv $dst.wap.opt .v $dst.wap .cnf) &>> $dst.log
	(time $bin/optimize.native 401 421 $dst.wap.cnf) &>> $dst.log
	(time $bin/test_icp_post2.native $dst.wap.con $dst.wap.cnf.out | grep "\[" | tr '[]; ' '\n' | grep . | tr '\n' ' '> $dst.wap-order) &>> $dst.log
	$bin/test_reorder_cnf.native $src $dst.wap-order $dst.wap-order.cnf
	$bin/test_reorder_cnf.native $src $dst.wap-order $dst.wap-rev-order.cnf -rev
	(time $conv $dst.wap-order .cnf $dst.wap-order .cp.A.pure $dst.wap-order .stats) &>> $dst.log
	python $extra/pick.py --strip "()[]:# \"" 4 """$(cat $dst.wap-order.stats)""" >> $dst.out
	(time $conv $dst.wap-rev-order .cnf $dst.wap-rev-order .cp.A.pure $dst.wap-rev-order .stats) &>> $dst.log
	python $extra/pick.py --strip "()[]:# \"" 4 """$(cat $dst.wap-rev-order.stats)""" >> $dst.out
	echo "done =)!"
done
