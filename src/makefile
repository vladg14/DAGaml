#NUMS=/usr/lib/ocaml/nums
LibNum=~/.opam/system/lib/num/nums
LibUnix=/usr/lib64/ocaml/unix
LIBS=-libs $(LibNum),$(LibUnix)
#LIBS=-lib /usr/lib/ocaml/nums
SRCS=-Is tools,src,bitv,grobdd,io,conv,oops,circuits
NPROC=$(shell nproc)
OcamlBuild=~/.opam/system/bin/ocamlbuild
#OcamlBuild=ocamlbuild
OB=$(OcamlBuild) -r -j $(NPROC) $(LIBS) $(SRCS)
MV=mv *.native *.byte ../bin/ &> /dev/null
.PHONY: all conv tests


all: tests

tests:
	$(OB)\
		tools/bigInt.native \
		tools/hBTree.native \
		src/wUbdag.native \
		test/test_distrib.native \
		test/test_split_output.native \
		test/test_reorder_cnf.native \
		test/test_extract_dgraph_from_cnf.native \
		test/optimize.native \
		test/optimize.d.byte \
		tools/hTree.native \
		tools/genMetaWHashcache.native \
		tools/ephKn2.native \
		tools/ephKn2Hashtbl.native \
		tools/wMemoTable.native \
		tools/hTree.native \
		test/test_icp_post2.native \
		test/test_vuint_exp_card.native \
		test/test_teval.native \
		test/test_icp_post.native \
		test/test_icp.native \
		test/test_tcqueens_cnax.native \
		test/test_tcqueens_cp.native \
		circuits/cnax.native \
		circuits/nax.native \
		circuits/aig.native \
		conv/conv.native \
		test/test_conv.native \
		test/test_conv_ext.native
	$(MV)


conv:
	$(OB) \
		test/test_finite_solver_cpx2B.native \
		test/test_conv.native \
		test/test_conv_ext.native \
	$(MV)

extra:
	$(OB) \
		oops/vUInt.native \
		conv/conv.native \
		test/test_upgrade_tacx_cp_to_cpB.native \
		test/test_upgrade_pure_cpB_to_cpx2B.native \
		test/test_test_cpx2B.native \
		test/test_test_cpx2B.d.byte \
		test/test_noreg_cpx2B_cons.native \
		grobdd/cpx2BDumpLoadTest.native \
		grobdd/cpx2BGopsTest.native \
		grobdd/cpx2BUtilsTest.native \
		test/test_check_tacx_cpx2B.native \
		test/test_check_tacx_cpx2B.d.byte \
		test/test_upgrade_tacx_cpB_to_nniB.native \
		test/test_upgrade_tacx_cpB_to_nniB.d.byte \
		test/test_eval_tacx_to_pure_nniB.native \
		test/test_eval_tacx_to_pure_nniB.d.byte \
		test/test_eval_tacx_to_pure_cpxB.native \
		test/test_eval_tacx_to_pure_cpxB.d.byte \
		test/test_upgrade_tacx_cpB_to_cpxB.native \
		src/binUbdag.native \
		src/binUbdagT.native \
		src/binUbdagTE.native \
		src/binUbdagTC.native \
		grobdd/cpB.native \
		grobdd/bryantB.native \
		grobdd/zddB.native \
		grobdd/nniB.native \
		grobdd/cpx2B.native \
		src/taggedSubdagPropa.native \
		test/test_test_cpx.d.byte \
		test/test_noreg_cpx_cons.native \
		test/test_noreg_cpx2_cons.native \
		test/test_bin_of_int.native \
		test/test_downgrade_pure_cpx_to_bryant.native \
		grobdd/cpx2UtilsTest.native \
		grobdd/cpx2DumpLoadTest.native \
		grobdd/cpx2GopsTest.native \
		test/test_upgrade_benchmark_cnf_to_tacx_cpB.native
	$(MV)

extraephe:
	$(OB) \
		ephe/udag.native \
		ephe/ubdag.native \
		ephe/subdag.native
	$(MV)

lucas:
	$(OB) \
		test/test_extract_dgraph_from_cnf.native
	$(MV)

clean:
	$(OcamlBuild) -clean
	rm -rf _build
	rm -f bin/*.native bin/*.byte *.native *.d.byte
