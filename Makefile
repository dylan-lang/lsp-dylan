# NOTE: This depends on the dylan-compiler -unify flag, so requires a release
#       after 2020.1.

DYLAN		?= $${HOME}/dylan
install_bin     = $(DYLAN)/bin
app_name	= dylan-lsp-server

build: *.dylan
	dylan-compiler -build -unify $(app_name)

install: build
	mkdir -p $(install_bin)
	cp _build/sbin/$(app_name) $(install_bin)/

install-debug: build
	mkdir -p $(install_bin)
	cp _build/sbin/$(app_name).dbg $(install_bin)/$(app_name)

test: build
	dylan-compiler -build lsp-dylan-test-suite
	_build/bin/lsp-dylan-test-suite

clean:
	rm -rf _build

distclean: clean
	rm -f $(install_bin)/$(app_name)
