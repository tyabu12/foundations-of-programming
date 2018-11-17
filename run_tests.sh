#!/bin/bash

set -eo pipefail

find . -mindepth 2 -name "ex*.ml" | while read line;
  do
    if [ `echo $line | grep 'metro'` ]; then
      ocamlc -o a.out metro.ml $line && ./a.out
    else
      ocaml $line
    fi
  done

