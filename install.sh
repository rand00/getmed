#! /bin/bash

ocamlbuild -use-ocamlfind main.native
cp main.native ~/bin/getmed

