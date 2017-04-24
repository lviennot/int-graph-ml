TARGETS:=test
MODS:=traversal.ml
LIBS:=-lib bigarray
# -lib str -lib unix
OPTS:=-cflags -ccopt,-O3
#  -lib graphics -cflags -I,+lablgtk
#PACKAGES:=-package ocamlgraph -package batteries
APIDOC:=GenArray Heap EdgeArray IntGraph LabelSet Traversal Skeleton
SRCS:=$(wildcard src/*.ml) $(wildcard src/*.mli)


binaries: _tags
	 ocamlbuild $(OPTS) $(LIBS) $(PACKAGES) $(patsubst %,%.native,$(TARGETS))


all: binaries api.doc


.PHONY: _tags

_tags:
	(echo "true:      inline(3)" ; echo "true:       debug" ; echo "<src>:      include") > _tags

%.native: _tags $(SRCS)
	ocamlbuild $(OPTS) $(LIBS) $(PACKAGES) $@

unit: _tags $(SRCS)
	ocamlbuild $(LIBS) $(PACKAGES) unit.native --


mods.cma: $(patsubst %,src/%,$(MODS))
	echo $(patsubst %,src/%,$(MODS)) > mods.mllib
	ocamlbuild $(LIBS) $(PACKAGES) $@

mods.top: $(patsubst %,_src/%,$(MODS))
	echo $(patsubst %,_build/src/%,$(MODS)) > mods.mltop
	ocamlbuild $(LIBS) $(PACKAGES) $@

ocaml:
	utop || rlwrap ocaml || ocaml 



.PHONY: api.doc

api.doc: 
	rm -f $@ api.odocl
	for m in $(APIDOC) ; do echo $$m >> api.odocl ; done
	ocamlbuild -docflags -charset,UTF-8,-colorize-code,-html,-short-functors $(LIBS) $(PACKAGES) api.docdir/index.html
	rm -f api.odocl
	mv api.docdir $@


clean:
	rm -f *~ src/*~
	ocamlbuild -clean
	rm -fr _build _tags api.odocl mods.top mods.mltop
	rm -f _data/USA-road-t.NY.gr.gz


# ------------- test

test: _data/USA-road-t.NY.gr.gz test.native
	for s in 1 10 100 1000; do \
		gunzip -c $< | grep '^a ' | ./test.native $$s; \
	done

_data/USA-road-t.NY.gr.gz:
	curl -o $@ http://www.dis.uniroma1.it/challenge9/data/USA-road-t/USA-road-t.NY.gr.gz

# -------------- graphviz

GRAPHVIZ:=neato -Ksfdp -Goverlap=scale -Gsplines=curved -Nlabel="" -Earrowhead=none -Nshape=circle -Nstyle=filled -Nwidth=.1 -Ncolor="\#00000060" -Ecolor="\#00000020"
# -Nstyle=filled -Nheight=1 -Nwidth=1 -Nfixedsize=true

%.pdf: %.dot
	$(GRAPHVIZ) -o $@ -Tpdf $<

%.svg: %.dot
	$(GRAPHVIZ) -o $@ -Tsvg $<



