scp $goonline:~/DAGaml/result.csv input-result-cnf.csv
grep "," input-result-cnf.csv | grep -v "\[" | sed "s/,//g" > result-cnf.csv
python synth_file.py result-cnf.csv | grep -v min | grep -v max | grep -v quartile | grep -v mediane > result-cnf.log
scp $goonline:~/DAGaml-1/result.csv input-result-v-10m.csv
grep "," input-result-v-10m.csv | grep -v "\[" | sed "s/,//g" > result-v-10m.csv
python synth_file.py result-v-10m.csv | grep -v min | grep -v max | grep -v quartile | grep -v mediane > result-v-10m.log
scp $goonline:~/DAGaml-2/result.csv input-result-v-30m.csv
grep "," input-result-v-30m.csv | grep -v "\[" | sed "s/,//g" > result-v-30m.csv
python synth_file.py result-v-30m.csv | grep -v min | grep -v max | grep -v quartile | grep -v mediane > result-v-30m.log


