timeout="timeout 2m"
mkdir workdir workdir/satlib
(cd workdir/satlib; mkdir uf100-430 uf200-860 uf50-218 uuf150-645 uuf250-1065 uf125-538 uf20-91 uf75-325 uuf175-753 uuf50-218 uf150-645 uf225-960 uuf100-430 uuf200-860 uuf75-325 uf175-753 uf250-1065 uuf125-538 uuf225-960) &> /dev/null
for file in $@
do
	workfile=$(python extract_name.py $file)
	echo "$file -> $workfile"
#	echo " V1"
#	echo " - upgrade"
#	(time ./test_upgrade_benchmark_cnf_to_tacx_cp.native $file $workfile.1.cp.tacx) 	&> $workfile.1.up.log
#	echo " - compile"
#	$timeout ./pure_of_tacx.sh --cp $workfile.1.cp.tacx --no-dot                        &> $workfile.1.eva.log
	echo " V2"
	echo " - upgrade"
	(time ./test_upgrade_benchmark_cnf_to_tacx_cp_v2.native $file $workfile.2.cp.tacx) 	&> $workfile.2.up.log
	echo " - compile"
	$timeout ./pure_of_tacx.sh --cp $workfile.2.cp.tacx --no-dot                        &> $workfile.2.eva.log
	echo " V3"
	echo " - upgrade"
	(time ./test_upgrade_benchmark_cnf_to_tacx_cp_v3.native $file $workfile.3.cp.tacx) 	&> $workfile.3.up.log
	echo " - compile"
	$timeout ./pure_of_tacx.sh --cp $workfile.3.cp.tacx --no-dot                        &> $workfile.3.eva.log
done
python select-lines.py "[4, 7, 15, 13, 49, 52]" workdir/satlib/uf50-218/uf50-0*.eva.log
