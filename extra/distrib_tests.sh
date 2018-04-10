mkdir -p workdir/distrib
echo "distrib n:" $1 "k:" $2
dst="workdir/distrib/dist-$1-$2"
conv="bin/test_conv_ext.native"
topdf="extra/dot_and_view.sh"
echo $dst > $dst.log
echo "gen $dst.v"
bin/test_distrib.native $1 $2 $dst.v
echo "gen .v -> .nu -> .dot"
time $conv $dst .v $dst .cp.B.pure $dst.nu .dot > /dev/null
grep N $dst.nu.dot | grep -v "\->" | wc -l >> $dst.log
echo "gen .nu -> .cntsat"
time $conv $dst .cp.B.pure $dst.nu .cntsat > /dev/null
cat $dst.nu.cntsat | sed "s/\[//" | sed "s/\]//" >> $dst.log
echo "gen .v -> .nux -> .dot"
time $conv $dst .v $dst .cpx.B.pure $dst.nux .dot > /dev/null
grep N $dst.nux.dot | grep -v "\->" | wc -l >> $dst.log
echo "gen .nux -> .cntsat"
time $conv $dst .cpx.B.pure $dst.nux .cntsat > /dev/null
cat $dst.nux.cntsat | sed "s/\[//" | sed "s/\]//" >> $dst.log
