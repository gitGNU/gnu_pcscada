#
#  Copyright (c) 2008,
#  Reto Buerki <buerki@swiss-it.ch>
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
INSTALL = install

VERSION = 0.5
GIT_REV = $(shell git-show --pretty="format:%h" | head -n1)
PCSCADA = libpcsc-$(VERSION)
DISTFILES = `ls | grep -v libpcsc`

SOURCEDIR = src
ALI_FILES = lib/*.ali
SO_LIBRARY = libpcscada.so.$(VERSION)

all: build_lib

build_lib: prepare
	@gnatmake -Ppcscada_lib -XPCSCADA_VERSION="$(VERSION)"

build_utests: prepare
	@gnatmake -Ppcscada_utests

build_itests: prepare
	@gnatmake -Ppcscada_itests

build_examples: prepare
	@gnatmake -Ppcscada_examples

prepare: $(SOURCEDIR)/pcsc-version.ads
	@mkdir -p obj/lib obj/itests obj/utests obj/examples lib

$(SOURCEDIR)/pcsc-version.ads:
	@echo "package PCSC.Version is"                        > $@
	@echo "   Version_Number : constant Float  :="        >> $@
	@echo "      $(VERSION);"                             >> $@
	@echo "   Version_String : constant String :="        >> $@
	@echo "     \"$(VERSION) (git $(GIT_REV))\";"         >> $@
	@echo "end PCSC.Version;"                             >> $@

clean:
	@rm -rf obj/*
	@rm -rf lib/*

distclean:
	@rm $(SOURCEDIR)/pcsc-version.ads
	@rm -rf obj
	@rm -rf lib

# run unit tests
utests: build_utests
	@obj/utests/runner

# run 'integration' tests
# you need a reader and smartcard for this to work
itests: build_itests
	@obj/itests/test_pcscada

# build all examples
examples: build_examples

install: install_lib

install_lib:
	@mkdir -p $(PREFIX)/include/pcscada
	@mkdir -p $(PREFIX)/lib/pcscada
	$(INSTALL) -m 644 $(SOURCEDIR)/* $(PREFIX)/include/pcscada
	$(INSTALL) -m 444 $(ALI_FILES) $(PREFIX)/lib/pcscada
	$(INSTALL) -m 444 lib/$(SO_LIBRARY) $(PREFIX)/lib/pcscada
	@ln -sf $(PREFIX)/lib/pcscada/$(SO_LIBRARY) $(PREFIX)/lib/libpcscada.so

docs:
	@ls $(SOURCEDIR)/*.ads > pcscada.specs
	@adabrowse -c adabrowse.cfg -p -t -i -I src/ -f@pcscada.specs -o doc/

.PHONY: itests utests
