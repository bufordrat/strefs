# strefs            -*- makefile-gmake -*-
# GNUmakefile

DISPLAY = short
DUNE = opam exec -- dune $1 --display $(DISPLAY)

build all:
	$(call DUNE, build @@default)
.PHONY: build all

doc:
	$(call DUNE, build @doc)
.PHONY: doc

clean:
	$(call DUNE, clean)
.PHONY: clean

sandbox:
	opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
PHONY: sandbox

deps:
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam install . --deps-only --yes
.PHONY: deps

dune-install: build
	eval $$(opam env)
	$(call DUNE, install)
.PHONY: install

dev-install: dune-install
	install -m 555 $(OPAM_SWITCH_PREFIX)/bin/strefs ~/bin
.PHONY: home-install

dev-uninstall: 
	$(RM) ~/bin/strefs
.PHONY: home-install

mounts:
	if mountpoint /data/web 2> /dev/null; then : ; else sudo mkdir -p /data/web && sudo mount voldemort:/export/www-legacy /data/web ; fi
.PHONY: mounts

publish: build mounts
	echo 'url { src:' $$(cat strefs.opam | grep dev-repo | awk '{ print $$2 }') '}' >> strefs.opam
	make -C /data/web/dldc/opam add NAME=strefs OPAM=$$PWD/strefs.opam
.PHONY: publish

# Local Variables:
# mode: makefile-gmake
# End:
