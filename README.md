[![Build stats](https://travis-ci.org/mirage/ocaml-gnt.png?branch=master)](https://travis-ci.org/mirage/ocaml-gnt)

Xen-style memory sharing implementations
========================================

This package builds the following subpackages:

xen-gnt.unix: implements memory sharing under Unix with mmap(2)
xen-gnt.xen: implements memory sharing via Xen grant tables
xen-gnt.xenctrl: implements memory sharing via libxc and Xen grant tables

To see a concrete example, have a look at [mirage/ocaml-vchan]
