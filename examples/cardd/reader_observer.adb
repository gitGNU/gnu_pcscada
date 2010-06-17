--
--  Copyright (c) 2008-2010,
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

with Ada.Text_IO;

with PCSC.SCard.Utils;

package body Reader_Observer is

   package Utils renames PCSC.SCard.Utils;

   -------------------------------------------------------------------------

   procedure Notify
     (O         : Instance;
      Condition : SCard.Reader_Condition)
   is
   begin
      Ada.Text_IO.Put_Line ("[observer (" & Utils.To_String
                            (States => O.States) & ") ]");
      Ada.Text_IO.Put_Line ("Reader : " & Utils.To_String (Condition.Name));
      Ada.Text_IO.Put_Line ("State  : " & Utils.To_String
                            (Condition.Current_State));
      if Condition.Current_State.Is_In (State => SCard.S_Reader_Present) then
         Ada.Text_IO.Put_Line ("ATR    : " & Utils.To_Hex_String
                               (Given => Condition.Card_ATR));
      end if;
      Ada.Text_IO.New_Line;
   end Notify;

end Reader_Observer;
