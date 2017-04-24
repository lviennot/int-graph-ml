# int-graph-ml
Compact graph data-structure in ocaml.

This library provides an ocaml graph implementation with pretty low memory
usage: a graph with `n` nodes and `m` edges is basically represented with `n+m`
words. Using bigarrays, unweighted graphs with less than `2^31` nodes can be
represented withing `8n+4m` bytes. Weighted graphs basically use an additional
word per edge (or only 4 bytes for Int32 weights).

## Install

You can install ocaml and required modules with opam, something like :
```
apt-get install opam
opam init --comp 4.04.0
eval `opam config env`
```

Then :
```
make
```

## Usage

See `src/test.ml` for an example.

Try it (on New York graph from 9th DIMACS) with:
```
make test
```

## Documentation

Try:
```
make api.docdir
```

## License

GNU LGPL.

