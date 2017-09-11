./bin/test_alex.native $1 alex.cp.B.tacx
time ./bin/test_conv_ext.native alex .cp.B.tacx alex .cp.B.pure > /dev/null
./bin/test_conv_ext.native alex .cp.B.pure alex .cntsat
cat alex.cntsat
./bin/test_conv_ext.native alex .cp.B.pure alex .dot
#./bin/extra/dot_and_view.sh alex.dot
