let "imax=$2-1"
cp $1 $1.0.tacx
for i in $(seq 0 $imax)
do
	echo "step: " $i
	let "j=i+1"
	time ./test_roundup1_tacx_cpx.native $1.$i.tacx $1.$j.tacx $1.$j.tacx.dot
done
cp $1.$2.tacx $1.tacx
