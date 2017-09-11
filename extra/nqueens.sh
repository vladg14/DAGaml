pblm="tcqueens"
for i in $(seq 2 8)
do
    echo "starting $i-queen(s)"
    echo "  -> generation of $i-$pblm.cp.tacx"
    ./test_$(echo $pblm)_cp.native $i workdir/$i-$pblm.cp.tacx &> workdir/$i-$pblm.make-cp.log
    echo "  -> generation of $i-$pblm.nni.tacx"
    ./test_upgrade_tacx_cp_to_nni.native workdir/$i-$pblm.cp.tacx workdir/$i-$pblm.nni.tacx workdir/$i-$pblm.nni.tacx.dot &> workdir/$i-$pblm.cp-to-nni.log
    echo "  -> generation of $i-$pblm.cpx.tacx"
    ./test_upgrade_tacx_cp_to_cpx.native workdir/$i-$pblm.cp.tacx workdir/$i-$pblm.cpx.tacx workdir/$i-$pblm.cpx.tacx.dot &> workdir/$i-$pblm.cp-to-cpx.log
    echo "  -> generation of $i-$pblm.cpx.propa.tacx"
    ./test_propa_tacx_cpx.native workdir/$i-$pblm.cpx.tacx workdir/$i-$pblm.cpx.propa.tacx workdir/$i-$pblm.cpx.propa.tacx.dot &> workdir/$i-$pblm.propa-cpx.log
    echo "  -> generation of $i-$pblm.cp.pure"
    ./pure_of_tacx.sh --cp workdir/$i-$pblm.cp.tacx   --no-dot &> workdir/$i-$pblm.solve-cp.log
    echo "  -> generation of $i-$pblm.nni.pure"
    ./pure_of_tacx.sh --nni workdir/$i-$pblm.nni.tacx --no-dot &> workdir/$i-$pblm.solve-nni.log
    echo "  -> generation of $i-$pblm.cpx.pure"
    ./pure_of_tacx.sh --cpx workdir/$i-$pblm.cpx.tacx --no-dot &> workdir/$i-$pblm.solve-cpx.log
    echo "  -> generation of $i-$pblm.cpx.propa.pure"
    ./pure_of_tacx.sh --cpx workdir/$i-$pblm.cpx.propa.tacx --no-dot &> workdir/$i-$pblm.solve-propa-cpx.log
done

echo "summarize solve-cp.log"
python nqueens-parser-cp.py  workdir/*-$pblm.solve-cp.log 
echo "summarize solve-nni.log"
python nqueens-parser-cp.py  workdir/*-$pblm.solve-nni.log 
echo "summarize solve-cpx.log"
python nqueens-parser-cpx.py workdir/*-$pblm.solve-cpx.log
echo "summarize solve-propa-cpx.log"
python nqueens-parser-cpx.py workdir/*-$pblm.solve-propa-cpx.log
