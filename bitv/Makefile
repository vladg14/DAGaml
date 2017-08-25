##########################################################################
#                                                                        #
#  Copyright (C) Jean-Christophe Filliatre                               #
#                                                                        #
#  This software is free software; you can redistribute it and/or        #
#  modify it under the terms of the GNU Library General Public           #
#  License version 2, with the special exception on linking              #
#  described in file LICENSE.                                            #
#                                                                        #
#  This software is distributed in the hope that it will be useful,      #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  #
#                                                                        #
##########################################################################

# where to install
LIBDIR = /usr/lib/ocaml/bitv
OCAMLFIND=/usr/bin/ocamlfind

CMO = bitv.cmo
CMI = $(CMO:.cmo=.cmi)
MLI = $(CMO:.cmo=.mli)
CMX = $(CMO:.cmo=.cmx)
OBJS = $(CMO:.cmo=.o)

BYTEFILES = $(CMO) $(CMI) $(MLI)
OPTFILES = $(CMX) $(OBJS)

all: opt test

byte: $(CMO)

opt: $(CMO) $(CMX)

test: bitv.cmo
	ocaml test.ml

install: install-opt

ifeq "yes" "no"

install-byte:
	mkdir -p $(DESTDIR)$(LIBDIR)
	cp -f $(BYTEFILES) $(DESTDIR)$(LIBDIR)

install-opt:
	mkdir -p $(DESTDIR)$(LIBDIR)
	cp -f $(BYTEFILES) $(DESTDIR)$(LIBDIR)
	cp -f $(OPTFILES) $(DESTDIR)$(LIBDIR)

else

DESTDIR=-destdir $(LIBDIR:/bitv=)

install-byte:
	$(OCAMLFIND) remove $(DESTDIR) bitv
	$(OCAMLFIND) install $(DESTDIR) bitv META $(BYTEFILES)

install-opt:
	$(OCAMLFIND) remove $(DESTDIR) bitv
	$(OCAMLFIND) install $(DESTDIR) bitv META $(BYTEFILES) $(OPTFILES)

endif

# l.p.
######

doc: bitv.pdf

bitv.tex: bitv.mli
	true -s --no-index bitv.mli -o $@

bitv.pdf: bitv.tex
	pdflatex bitv && pdflatex bitv

# export
########

VERSION=1.1
NAME=bitv-$(VERSION)

SOURCES = bitv.mli bitv.ml test.ml META.in Makefile.in configure configure.in \
	  .depend README COPYING LICENSE CHANGES

FTP=/users/demons/filliatr/WWW/ftp/ocaml/ds

export:: bitv.pdf $(SOURCES)
	mkdir -p export/$(NAME)
	cp $(SOURCES) export/$(NAME)
	(cd export; tar cf $(NAME).tar $(NAME); \
	gzip -f --best $(NAME).tar)
	cp -f bitv.ml bitv.mli export/$(NAME).tar.gz $(FTP)
	cp -f bitv.pdf $(FTP)
	caml2html -d $(FTP) bitv.ml bitv.mli

# generic rules :
#################

CAMLC = ocamlc.opt
CAMLCOPT = ocamlopt.opt
FLAGS=

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(FLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(FLAGS) $<

.ml.o:
	$(CAMLCOPT) -c $(FLAGS) $<

.ml.cmx:
	$(CAMLCOPT) -c $(FLAGS) $<

# myself

Makefile: Makefile.in config.status
	./config.status

config.status: configure
	./config.status --recheck

configure: configure.in
	autoconf

# clean and depend
##################

clean:
	rm -f *~ *.cm[iox] *.o
	rm -f bitv.tex *.aux *.log *.dvi *.pdf

depend:
	rm -f .depend
	ocamldep *.mli *.ml > .depend

include .depend

