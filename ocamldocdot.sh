#! /bin/bash

ocamlfind ocamldoc \
          -package yojson,ppx_deriving_yojson,ppx_deriving_yojson.runtime \
          -package batteries,text,text.pcre,pcre,lambda-term \
          -package astring,rresult,ppx_deriving.show,containers,cmdliner \
          -I _build/src/ \
          -I _build/lib_batext/ \
          -dot \
          -d docs \
          src/*.ml

dot -Tpng -O ocamldoc.out \
&& feh ocamldoc.out.png


#    -hide BatExt_rand00,Pervasives \
#    lib_sc/src/*ml \
#    lib_batext_rand00/src/*.ml \
