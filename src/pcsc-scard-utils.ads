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

with PCSC.Thin;
with PCSC.SCard;

use PCSC;

--  SCard utilities package
package PCSC.SCard.Utils is

   type Callback is access procedure (ID : in Reader_ID := Null_Reader_ID);
   --  Callback for reader ID handling. Provides flexible way to access
   --  specific readers.

   function To_Hex_String (Given : in Byte_Set; Len : in Positive)
                           return String;
   --  Returns hex-representation string of binary data (Byte_Set). Len defines
   --  the length of the returned string.

   function To_Hex_String (Given : in Byte_Set) return String;
   --  Returns hex-representation string of binary data (Byte_Set). The string
   --  length will be the double length of the initial byte set.

   function To_Hex_String (Given : in Thin.Byte_Array; Len : in Positive)
                           return String;
   --  Returns hex-representation string of binary data (Byte_Array). Len
   --  defines the length of the returned string.

   function To_Hex_String (Given : in ATR) return String;
   --  Returns hex-representation string of an ATR.

   function To_String (Reader : in Reader_ID := Null_Reader_ID)
                       return String;
   --  Return string from Reader_ID.

   function To_String (States : in Card_States_Set) return String;
   --  Return string representation of card reader states.

   function To_String (States : in Reader_States_Set) return String;
   --  Return string representation of reader states.

   function To_String (Given : in Byte_Set) return String;
   --  Return an array of characters from Byte_Set.

   procedure For_Every_Reader
     (Readers : in Reader_ID_Set;
      Call    : in Callback);
   --  Call callback procedure for every reader in readers list.

   procedure Print_ReaderID (ID : in Reader_ID);
   --  Print out specific reader ID to default output.

end PCSC.SCard.Utils;
