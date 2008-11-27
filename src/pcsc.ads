--
--  Copyright (c) 2008,
--  Reto Buerki <reet@codelabs.ch>
--
--  This file is part of PCSC/Ada.
--
--  PCSC/Ada is free software; you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Public License as published
--  by the Free Software Foundation; either version 2.1 of the License, or
--  (at your option) any later version.
--
--  PCSC/Ada is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with PCSC/Ada; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
--  MA  02110-1301  USA
--
--  <PURPOSE>
--    This library provides bindings to PC/SC-middleware for the Ada
--    programming language. The library allows you to communicate with smart
--    cards using the SCard API with Ada.
--  </PURPOSE>
--

package PCSC is

   SCard_Error : exception;
   --  Raised by all PCSC.SCard routines in case of PC/SC SCard errors.

   SCard_Not_Supported : exception;
   --  Raised if an operation has been requested which is not supported by
   --  the reader or card.

   Bytes_Too_Big : exception;
   --  Exception will be raised by utils and helper functions/procedures if a
   --  given SCard.Byte_Set cannot be converted to a specific type because it
   --  contains more bytes than the target type can store.

end PCSC;
