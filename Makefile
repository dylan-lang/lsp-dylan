# Low-tech Makefile to build and install lsp-dylan.

# Because there's currently no way to make a static executable (until the
# release following Open Dylan 2020.1) this gets installed with the following
# directory structure:
#
#   ${DYLAN}/install/lsp-dylan/bin/dylan-lsp-server   # executable
#   ${DYLAN}/install/lsp-dylan/lib/*                  # shared libraries
#   ${DYLAN}/bin/dylan-lsp-server                     # symlink
#      -> ../install/lsp-dylan/bin/dylan-lsp-server   #   to here
#
# So just make sure ${DYLAN}/bin (or ${HOME}/dylan/bin, the default) is on your
# PATH.

DYLAN		?= $${HOME}/dylan
install_dir     = $(DYLAN)/install/lsp-dylan
install_bin     = $(install_dir)/bin
install_lib     = $(install_dir)/lib
link_target     = $(install_bin)/dylan-lsp-server
link_source     = $(DYLAN)/bin/dylan-lsp-server

build: *.dylan
	cd .. && dylan-compiler -build dylan-lsp-server

install: build
	mkdir -p $(install_bin)
	mkdir -p $(install_lib)
	cp ../_build/bin/dylan-lsp-server $(install_bin)/
	cp -r ../_build/lib/lib* $(install_lib)/
	mkdir -p $(DYLAN)/bin
	rm -f $(link_source)
	ln -s $(link_target) $(link_source)

# Build with packages, assumes `dylan` executable is on the PATH.
test: build
	dylan update
	cd .. && dylan-compiler -build lsp-dylan-test-suite \
	      && _build/bin/lsp-dylan-test-suite

clean:
	rm -rf ../_build

distclean: clean
	rm -rf $(install_dir)
	rm -f $(link_source)
