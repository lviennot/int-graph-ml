TARGETS:=test
MODS:=traverse.ml
LIBS:=-lib bigarray
# -lib str -lib unix
OPTS:=-cflags -ccopt,-O3
#  -lib graphics -cflags -I,+lablgtk
#PACKAGES:=-package ocamlgraph -package batteries
# -package batteries 
APIDOC:=IntDigraph Vector Traversal Skeleton Sig Diameter
SRCS:=$(wildcard src/*.ml) $(wildcard src/*.mli)


binaries: _tags
	 ocamlbuild $(OPTS) $(LIBS) $(PACKAGES) $(patsubst %,%.native,$(TARGETS))

test: test.native
	for s in 1 10 100 1000; do \
		gunzip -c ../graph/_data/USA-road-t.NY.gr.gz | grep '^a ' | ./test.native $$s > /tmp/o; \
	done

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


-include viz.make manage.make example.make

