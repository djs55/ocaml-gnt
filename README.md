[![Build stats](https://travis-ci.org/mirage/ocaml-gnt.png?branch=master)](https://travis-ci.org/mirage/ocaml-gnt)

Xen-style memory sharing implementations
========================================

This package builds the following subpackages:

- xen-gnt.unix: implements memory sharing under Unix with mmap(2)
  - (via module Unix_memory : S.MEMORY)
- xen-gnt.xen: implements memory sharing via Xen grant tables
  - (via module Xen_memory : S.MEMORY)
- xen-gnt.xenctrl: implements memory sharing via libxc and Xen grant tables
  - (via module Xenctrl_memory : S.MEMORY)

To see a concrete example, have a look at [mirage/ocaml-vchan]
