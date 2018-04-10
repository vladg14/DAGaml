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
	cp $src $dst.v
	$bin/test_split_output.native $dst .v
done
