# NOTE: This depends on the dylan-compiler -unify flag, so requires a release
#       after 2020.1.

DYLAN		?= $${HOME}/dylan
install_bin     = $(DYLAN)/bin
app_name	= dylan-lsp-server

build: sources/*.dylan sources/lsp-dylan.lid sources/server.lid
	deft build --unify $(app_name)

install: build
	mkdir -p $(install_bin)
	if [[ -d "_build" ]]; then                       \
	  cp _build/sbin/$(app_name) $(install_bin)/;    \
	else                                             \
	  cp ../_build/sbin/$(app_name) $(install_bin)/; \
	fi

test: sources/*-tests.dylan sources/test-suite*.dylan sources/test-suite.lid
	deft build lsp-dylan-test-suite
	if [[ -d "_build" ]]; then             \
	  _build/bin/lsp-dylan-test-suite ;    \
	else                                   \
	  ../_build/bin/lsp-dylan-test-suite ; \
	fi

clean:
	rm -rf _build

distclean: clean
	rm -f $(install_bin)/$(app_name)
