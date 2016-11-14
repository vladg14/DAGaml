#NUMS=/usr/lib/ocaml/nums
#LIBS=-lib /usr/lib64/ocaml/nums
LIBS=-lib /usr/lib/ocaml/nums
SRCS=
OB=ocamlbuild -j ${NPROC} -r $(LIBS) $(SRCS)

TARGETS=$(patsubst %.ml,%.native,$(wildcard *.ml))

all:
	echo $(TARGETS)
	$(OB) $(TARGETS)

clean:
	ocamlbuild -clean
	rm -rf _build
	rm -f *.native *.d.byte
