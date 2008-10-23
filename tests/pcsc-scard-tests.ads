--
--  Copyright (c) 2008,
--  Reto Buerki <buerki@swiss-it.ch>
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

with Ahven.Framework;

--  Test package for PCSC.SCard functions and procedures.
package PCSC.SCard.Tests is

   type Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Test);
   --  Initialize Test suite.

   procedure Test_Slice_Readerstring;
   --  Test Slice_Readerstring function.

   procedure Test_To_C_RStatus_Set;
   --  Test Reader_Status_Set to Thin.READERSTATE_Array conversion.

   procedure Test_To_Chars_Ptr;
   --  Test Reader_ID to chars_ptr conversion.

   procedure Test_To_Ada_Proto;
   --  Test Thin.DWORD to Proto conversion.

end PCSC.SCard.Tests;
