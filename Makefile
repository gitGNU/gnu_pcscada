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

all:
	@mkdir -p obj
	@gnatmake -Ppcsc_ada

clean:
	@rm -rf obj/*

distclean:
	@rm -rf obj

tests:
	@obj/thin_tests
	@obj/thick_tests

.PHONY: tests
