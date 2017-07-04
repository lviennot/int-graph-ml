TARGETS:=test gtfsTest
MODS:=GenArray Heap EdgeArray IntGraph LabelSet Skeleton Traversal Rows Gtfs
LIBS:=-lib bigarray
# -lib str -lib unix
OPTS:=-cflags -ccopt,-O3
#  -lib graphics -cflags -I,+lablgtk
#PACKAGES:=-package ocamlgraph -package batteries
APIDOC:=GenArray Heap EdgeArray IntGraph LabelSet Traversal Skeleton
SRCS:=$(wildcard src/*.ml) $(wildcard src/*.mli)


binaries: _tags
	 ocamlbuild $(OPTS) $(LIBS) $(PACKAGES) $(patsubst %,%.native,$(TARGETS))


all: binaries api.docdir

.PHONY: _tags

_tags:
	(echo "true:      inline(0)" ; echo "true:       debug" ; echo "<src> or <src/gtfs>:      include") > _tags

%.native: _tags $(SRCS)
	ocamlbuild $(OPTS) $(LIBS) $(PACKAGES) $@

unit: _tags $(SRCS)
	ocamlbuild $(LIBS) $(PACKAGES) unit.native --

_tags0:
	(echo "true:      inline(0)" ; echo "true:       debug" ; echo "<src> or <src/gtfs>:      include") > _tags

byte: _tags0
	 ocamlbuild $(OPTS) $(LIBS) $(PACKAGES) $(patsubst %,%.byte,$(TARGETS))
%.byte: _tags $(SRCS)
	ocamlbuild $(OPTS) $(LIBS) $(PACKAGES) $@


mods.cma: $(SRCS)
	echo $(MODS) > mods.mllib
	ocamlbuild $(LIBS) $(PACKAGES) $@

mods.info:
	ocamlobjinfo _build/mods.cma

mods.top: $(SRCS)
	echo $(MODS) > mods.mltop
	ocamlbuild $(LIBS) $(PACKAGES) $@

ocaml:
	utop || rlwrap ocaml || ocaml 



.PHONY: api.docdir

api.docdir: 
	rm -f $@ api.odocl
	for m in $(APIDOC) ; do echo $$m >> api.odocl ; done
	ocamlbuild -docflags -charset,UTF-8,-colorize-code,-html,-short-functors $(LIBS) $(PACKAGES) api.docdir/index.html
	rm -f api.odocl


clean:
	rm -f *~ src/*~ src/gtfs/*~
	ocamlbuild -clean
	rm -f *.native
	rm -fr _build _tags api.odocl mods.top mods.mltop mods.mllib
	rm -fr _data


# ------------- test

test: _data/USA-road-t.NY.gr.gz test.native
	for s in 1 12 103 1004 10005 100006; do \
		gunzip -c $< | grep '^a ' | ./test.native $$s; \
	done

_data/USA-road-t.NY.gr.gz:
	mkdir -p _data
	curl -o $@ http://www.dis.uniroma1.it/challenge9/data/USA-road-t/USA-road-t.NY.gr.gz


# -------------- graphviz

GRAPHVIZ:=neato -Ksfdp -Goverlap=scale -Gsplines=curved -Nlabel="" -Earrowhead=none -Nshape=circle -Nstyle=filled -Nwidth=.1 -Ncolor="\#00000060" -Ecolor="\#00000020"
# -Nstyle=filled -Nheight=1 -Nwidth=1 -Nfixedsize=true

%.pdf: %.dot
	$(GRAPHVIZ) -o $@ -Tpdf $<

%.svg: %.dot
	$(GRAPHVIZ) -o $@ -Tsvg $<


# --------------- GTFS

T1:=$(if $(T1),$(T1),07:00:00)
T2:=$(if $(T2),$(T2),11:00:00)

gtfs: /tmp/_gtfs_stif gtfsTest.native
	./gtfsTest.native $< 20170626 $(T1) $(T2)

gtfs_ratp: ../../dev/ratp/_gtfs_ratp gtfsTest.native
	./gtfsTest.native $< 20170606  $(T1) $(T2)

gtfs_stif: ../../dev/ratp/_gtfs_stif gtfsTest.native
	./gtfsTest.native $< 20170626 $(T1) 20170626 $(T2) projection | gzip -c > /tmp/proj.gr.gz

%.gtfs_routes_seq: gtfsTest.native
	./gtfsTest.native $* 20010101 00:00:00 30000101 26:00:00 routes_seq > routes_seq.txt

# 2> errors.txt

/tmp/_gtfs_stif:
	mkdir -p $@
	curl -o /tmp/stif.zip https://opendata.stif.info/explore/dataset/offre-horaires-tc-gtfs-idf/files/f24cf9dbf6f80c28b8edfdd99ea16aad/download/
	cd $@ ; unzip /tmp/stif.zip
	rm -f /tmp/stif.zip
