#NUMS=/usr/lib/ocaml/nums
#LIBS=-lib /usr/lib64/ocaml/nums
#LIBS=-lib /usr/lib/ocaml/nums
SRCS=-Is tools,src
NPROC=$(shell nproc)
OB=ocamlbuild -j $(NPROC) -r $(LIBS) $(SRCS)

all:
	$(OB) \
		test/test_udag.native \
		test/test_urdag.native \
		test/test_ubdag.native

clean:
	ocamlbuild -clean
	rm -rf _build
	rm -f *.native *.d.byte
