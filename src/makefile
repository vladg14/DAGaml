#NUMS=/usr/lib/ocaml/nums
LIBS=-lib /usr/lib64/ocaml/nums
#LIBS=-lib /usr/lib/ocaml/nums
SRCS=-Is tools,src,bitv
NPROC=$(shell nproc)
OB=ocamlbuild -r -j $(NPROC) $(LIBS) $(SRCS)
MV=mv *.native *.byte ../bin/ &> /dev/null
.PHONY: all tests prod extra lucas extraephe clean conv

all:
	$(OB) \
		src/binUbdag.native \
		src/binUbdagT.native \
		src/binUbdagTC.native \
		src/binUbdagTE.native \
		src/reduce.native \
		src/subdag.native \
		src/taggedSubdag.native \
		src/taggedSubdagPropa.native \
		src/ubdag.native \
		src/udag.native \
		src/urdag.native \
		src/utils.native \
		src/utilsDump.native
	$(MV)

clean:
	ocamlbuild -clean
	rm -rf _build
	rm -f bin/*.native bin/*.byte *.native *.d.byte
