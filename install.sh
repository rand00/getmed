#! /bin/bash

ocamlbuild -use-ocamlfind main.native
cp main.native ~/scripts/getmed

