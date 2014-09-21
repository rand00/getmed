#! /bin/bash

#important to use -Is instead of -I
ocamlbuild -use-ocamlfind \
    -Is core_rand \
    getmed.native
