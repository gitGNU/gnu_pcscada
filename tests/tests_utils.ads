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

package Tests_Utils is

   type Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Test);
   --  Initialize Test suite.

   procedure Test_To_Long_Long_Integer;
   --  Test Byte_Set to Long_Long_Integer conversion.

   procedure Test_Byte_Set_To_String;
   --  Test Byte_Set to String conversion.

   procedure Test_RStates_Set_To_String;
   --  Test Reader_States_Set to String conversion.

   procedure Test_CStates_Set_To_String;
   --  Test Card_States_Set to String conversion.

   procedure Test_ReaderID_To_String;
   --  Test Reader_ID to String conversion.

   procedure Test_ATR_To_Hex_String;
   --  Test ATR to HEX String conversion.

end Tests_Utils;
