if		[ $1 = "--cp" ]
then
		echo "GroBdd : CP"
		time ./test_eval_tacx_to_pure_cp.native $2 $2.pure || exit
elif	[ $1 = "--nni" ]
then
		echo "GroBdd : NNI"
		time ./test_eval_tacx_to_pure_nni.native $2 $2.pure || exit
elif	[ $1 = "--cpx" ]
then
		echo "GroBdd : CPX"
		time ./test_eval_tacx_to_pure_cpx.native $2 $2.pure || exit
else
		echo "select --cp or --nni or --cpx"
		exit
fi
echo "conversion pure -> dot"
time ./test_eval_pure_to_dot.native $1 $2.pure $2.pure.dot || exit
if [ $3 = "--no-dot" ]
then
	echo "do not convert dot to svg"
else
	time dot -Tsvg -O $2.pure.dot || exit
	if [ $3 = "--no-view" ]
	then
		echo "do not display the svg file"
	else
		viewnior $2.pure.dot.svg || exit
	fi
fi
