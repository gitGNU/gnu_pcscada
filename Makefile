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

PREFIX?=$(HOME)/libraries
INSTALL=install

VERSION=$(shell grep " Version" src/pcsc.ads | cut -d\" -f2)
PCSCADA=libpcsc-$(VERSION)
DISTFILES=`ls | grep -v libpcsc`

SOURCES=src/*
ALI_FILES=lib/*.ali
SO_LIBRARY=libpcscada.so.$(VERSION)

all: build_lib

build_lib: create_dirs
	@gnatmake -Ppcscada_lib

build_utests: create_dirs
	@gnatmake -Ppcscada_utests

build_itests: create_dirs
	@gnatmake -Ppcscada_itests

create_dirs:
	@mkdir -p obj lib

clean:
	@rm -rf obj/*
	@rm -rf lib/*

distclean:
	@rm -rf obj
	@rm -rf lib

# run unit tests
utests: build_utests
	@obj/runner

# run 'integration' tests
# you need a reader and smartcard for this to work
itests: build_itests
	@obj/test_pcscada

install: install_lib

install_lib:
	@mkdir -p $(PREFIX)/include/pcscada
	@mkdir -p $(PREFIX)/lib/pcscada
	$(INSTALL) -m 644 $(SOURCES) $(PREFIX)/include/pcscada
	$(INSTALL) -m 444 $(ALI_FILES) $(PREFIX)/lib/pcscada
	$(INSTALL) -m 444 lib/$(SO_LIBRARY) $(PREFIX)/lib/pcscada
	@ln -sf $(PREFIX)/lib/pcscada/$(SO_LIBRARY) $(PREFIX)/lib/libpcscada.so

.PHONY: itests utests
