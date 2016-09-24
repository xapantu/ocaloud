all: build

build:
	ocamlbuild -use-ocamlfind -plugin-tags "package(eliom.ocamlbuild)" \
                                  server/ocaloudcore.cma server/ocaloudcore.cmxa server/ocaloudcore.cmxs \
                                  client/ocaloudcore.js -use-menhir

run: build
	mkdir -p _run/log/ocaloudcore/
	mkdir -p _run/data/ocaloudcore/ocsipersist
	ocsigenserver -c ocaloud.conf -v

clean:
	rm -rf _build
