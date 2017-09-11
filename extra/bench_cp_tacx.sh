for file in $@
do
	workfile=$(python extract_name.py $file)
	echo "$file -> $workfile"
	echo "    -  cp.tacx to nni.tacx"
	(time ./test_upgrade_tacx_cp_to_nni.native $workfile.cp.tacx $workfile.nni.tacx)	&> $workfile.cp-to-nni.log
	echo "    -  cp.tacx to cpx.tacx"
	(time ./test_upgrade_tacx_cp_to_cpx.native $workfile.cp.tacx $workfile.cpx.tacx)	&> $workfile.cp-to-cpx.log
	echo "    -  cp.tacx to  cp.pure"
	./pure_of_tacx.sh --cp $workfile.cp.tacx --no-dot 					&> $workfile.solve-cp.log
	echo "    - nni.tacx to nni.pure"
	./pure_of_tacx.sh --nni $workfile.nni.tacx --no-dot					&> $workfile.solve-nni.log
	echo "    - cpx.tacx to cpx.pure"
	./pure_of_tacx.sh --cpx $workfile.cpx.tacx --no-dot					&> $workfile.solve-cpx.log
	echo "    -  cp.pure to bryant.pure"
	(time ./test_downgrade_pure_cp_to_bryant.native $workfile.cp.tacx.pure $workfile.bryant.pure)	&> $workfile.solved-cp-to-bryant.log
	echo "    -  cp.pure to zdd.pure"
	(time ./test_downgrade_pure_cp_to_zdd.native $workfile.cp.tacx.pure $workfile.zdd.pure)	&> $workfile.solved-cp-to-zdd.log
	echo "    -  cp.pure to nni.pure"
	(time ./test_upgrade_pure_cp_to_nni.native $workfile.cp.tacx.pure $workfile.nni.pure)	&> $workfile.solved-cp-to-nni.log
	echo "    -  cp.pure to cpx.pure"
	(time ./test_upgrade_pure_cp_to_cpx.native $workfile.cp.tacx.pure $workfile.cpx.pure)	&> $workfile.solved-cp-to-cpx.log
	echo "    -  summarize"
	echo $workfile > $workfile.stats.log
	./test_pure_stats.native --bryant $workfile.bryant.pure	  >> $workfile.stats.log
	./test_pure_stats.native --zdd    $workfile.zdd.pure	  >> $workfile.stats.log
	./test_pure_stats.native --cp     $workfile.cp.tacx.pure  >> $workfile.stats.log
	./test_pure_stats.native --nni    $workfile.nni.pure      >> $workfile.stats.log
	./test_pure_stats.native --nni    $workfile.nni.tacx.pure >> $workfile.stats.log
	./test_pure_stats.native --cpx    $workfile.cpx.pure      >> $workfile.stats.log
	./test_pure_stats.native --cpx    $workfile.cpx.tacx.pure >> $workfile.stats.log
done
