#
#  Copyright (c) 2008,
#  Reto Buerki <reet@codelabs.ch>
#
#  This file is part of PCSC/Ada.
#
#  PCSC/Ada is free software; you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as published
#  by the Free Software Foundation; either version 2.1 of the License, or
#  (at your option) any later version.
#
#  PCSC/Ada is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public License
#  along with PCSC/Ada; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
#  MA  02110-1301  USA
#

PREFIX ?= $(HOME)/libraries
INCDIR  = $(PREFIX)/share/ada/adainclude/pcscada
ALIDIR  = $(PREFIX)/lib/ada/adalib/pcscada

INSTALL = install

MAJOR   = 0
MINOR   = 6
VERSION = $(MAJOR).$(MINOR)
GIT_REV = `git describe`
PCSCADA = libpcscada-$(VERSION)

SOURCEDIR  = src
APIDOCDIR  = doc
ALI_FILES  = lib/*.ali
SO_LIBRARY = libpcscada.so.$(VERSION)
A_LIBRARY  = libpcscada.a

TMPDIR  = /tmp
DISTDIR = $(TMPDIR)/$(PCSCADA)
TARBALL = $(PCSCADA).tar.bz2

LIBRARY_KIND = dynamic

all: build_lib

build_lib: prepare
	@gnatmake -p -Ppcscada_lib -XPCSCADA_VERSION="$(VERSION)" \
		-XLIBRARY_KIND="$(LIBRARY_KIND)"

build_utests: prepare
	@gnatmake -p -Ppcscada_utests

build_itests: prepare
	@gnatmake -p -Ppcscada_itests

build_examples: prepare
	@gnatmake -p -Ppcscada_examples

prepare: $(SOURCEDIR)/pcsc-version.ads

pcscada-git-rev: FORCE
	@if [ -d .git ]; then \
		if [ -r $@ ]; then \
			if [ "$$(cat $@)" != "$(GIT_REV)" ]; then \
				echo $(GIT_REV) > $@; \
			fi; \
		else \
			echo $(GIT_REV) > $@; \
		fi \
	fi

$(SOURCEDIR)/pcsc-version.ads: pcscada-git-rev
	@echo "package PCSC.Version is"                 > $@
	@echo "   Version_Number : constant Float  :=" >> $@
	@echo "      $(VERSION);"                      >> $@
	@echo "   Version_String : constant String :=" >> $@
	@echo "     \"$(VERSION) ($(GIT_REV))\";"      >> $@
	@echo "end PCSC.Version;"                      >> $@

clean:
	@rm -rf obj/*
	@rm -rf lib/*

distclean:
	@rm -rf obj
	@rm -rf lib
	@rm -rf $(APIDOCDIR)
	@rm -f pcscada.specs
	@rm -f $(SOURCEDIR)/pcsc-version.ads

# run unit tests
utests: build_utests
	@obj/utests/runner

# run 'integration' tests
# you need a reader and smartcard for this to work
itests: build_itests
	@obj/itests/test_pcscada

examples: build_examples

install: install_lib install_$(LIBRARY_KIND)

install_lib: build_lib
	@mkdir -p $(INCDIR)
	@mkdir -p $(ALIDIR)
	$(INSTALL) -m 644 $(SOURCEDIR)/* $(INCDIR)
	$(INSTALL) -m 444 $(ALI_FILES) $(ALIDIR)

install_static:
	$(INSTALL) -m 444 lib/$(A_LIBRARY) $(PREFIX)/lib

install_dynamic:
	$(INSTALL) -m 444 lib/$(SO_LIBRARY) $(PREFIX)/lib
	@cd $(PREFIX)/lib && \
		ln -sf $(SO_LIBRARY) libpcscada.so && \
		ln -sf $(SO_LIBRARY) libpcscada.so.$(MAJOR)

docs:
	@echo "Creating API doc for version $(VERSION) ..."
	@mkdir -p $(APIDOCDIR)
	@ls $(SOURCEDIR)/*.ads > pcscada.specs
	@adabrowse -c adabrowse.cfg -p -t -i -I src/ -f@pcscada.specs \
		-o $(APIDOCDIR)/

dist: distclean $(SOURCEDIR)/pcsc-version.ads docs
	@echo -n "Creating release tarball '$(PCSCADA)' ($(GIT_REV)) ... "
	@mkdir -p $(DISTDIR)
	@cp -R * $(DISTDIR)
	@tar -C $(TMPDIR) -cjf $(TARBALL) $(PCSCADA)
	@rm -rf $(DISTDIR)
	@echo "DONE"

FORCE:

.PHONY: dist itests utests
