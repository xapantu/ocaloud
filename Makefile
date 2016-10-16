all: build

build:
	ocamlbuild -use-ocamlfind -plugin-tags "package(eliom.ocamlbuild)" \
                                  server/ocaloudcore.cma server/ocaloudcore.cmxa server/ocaloudcore.cmxs \
                                  client/ocaloudcore.js -use-menhir

native: build
	ocamlbuild -mod server_main -use-ocamlfind -plugin-tags "package(eliom.ocamlbuild)" \
	server/ocaloudcore.native -use-menhir
	# Unfortunately, server_main needs to be at the end of the modules list, not
	# at the beginning, so we have to make a call manually.
	# Maybe there is an option in ocamlbuild to do that, did not find it unfortunately
	cd _build && ocamlfind ocamlopt -linkpkg -thread -package irmin -package irmin.unix -package inotify.lwt -package irc-client.lwt_ssl -package ocsigenserver -package ocsigenserver.ext.ocsipersist-dbm -package ocsigenserver.ext.staticmod -package sha -package eliom.server -package lwt.ppx -package react -package ppx_deriving_protobuf shared.cmx data.cmx markdown_ast.cmx markdown_parser.cmx markdown_lexer.cmx server/app_stub.cmx server/config.cmx server/user.cmx server/dumb_password.cmx server/widgets.cmx server/files.cmx server/irc_engine.cmx server/offline.cmx server/irc.cmx server/markdown.cmx server/mimes.cmx server/myform.cmx str_utils.cmx server/photos.cmx server/welcome.cmx server/ocaloudcore.cmx server_main.cmx -o server/ocaloudcore.native


run: build
	mkdir -p _run/log/ocaloudcore/
	mkdir -p _run/data/ocaloudcore/ocsipersist
	ocsigenserver -c ocaloud.conf -v

run.native: native
	mkdir -p _run/log/ocaloudcore/
	mkdir -p _run/data/ocaloudcore/ocsipersist
	./_build/server/ocaloudcore.native -c ocaloud.native.conf -v


clean:
	rm -rf _build
