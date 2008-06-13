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

package PCSC.Utils is

   function To_String (Given : Thin.Byte_Array; Len : Positive) return String;
   --  Returns hex-representation of binary data. Len defines the length
   --  of the returned string.

   function To_String (Reader : in SCard.Reader_ID) return String;
   --  Return string from Reader_ID.

   procedure For_Every_Reader
     (Readers : in SCard.Readers_List;
      Call    : in SCard.Callback);
   --  Call callback procedure for every reader in readers list.

end PCSC.Utils;
