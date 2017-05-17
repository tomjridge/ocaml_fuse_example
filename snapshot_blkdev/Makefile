SHELL:=/bin/bash
BASH_ENV:=bash_env.sh
export BASH_ENV

# targets ----------------------------------------

all: FORCE
	$$ocamlopt -g -linkpkg -o snapdev tjr_btree.cmxa snapdev.ml # FIXME why explicit ref to tjr_btree.cmxa? why isnt pkg enough?


# %.x: %.mli %.ml 
# 	$$ocamlc -c $*.mli $*.ml
# 	$$ocamlopt -c $*.mli $*.ml
# 	touch $@
# 
# %.x: %.ml
# 	$$ocamlc -c $*.ml
# 	$$ocamlopt -c $*.ml
# 	touch $@
# 
# tjr_lib.cma: $(xs)
# 	$$mk_cma -g -a -o $@ $(xs:.x=.cmo)
# 
# tjr_lib.cmxa: $(xs)
# 	$$mk_cmxa -g -a -o $@ $(xs:.x=.cmx)

clean: 
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa

FORCE:

