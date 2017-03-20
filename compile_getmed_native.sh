#! /bin/bash

#important to use -Is instead of -I
ocamlbuild -use-ocamlfind \
    -Is lib_batext \
    getmed.native
