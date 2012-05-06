#!/bin/sh
ocamlbuild -use-ocamlfind leveler.byte leveler.native scan.byte scan.native
