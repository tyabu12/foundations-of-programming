#!/bin/bash

set -eo pipefail

find . -mindepth 2 -name "ex*.ml" | while read line; do ocaml $line; done

