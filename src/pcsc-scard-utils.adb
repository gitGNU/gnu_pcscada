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

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Interfaces.C;

package body PCSC.SCard.Utils is

   -----------------------------
   --  To_String (Byte_Array) --
   -----------------------------

   function To_String (Given : Thin.Byte_Array; Len : Positive) return String
   is
      Hex : constant String := "0123456789ABCDEF";

      Result : String (1 .. Len);
      Where  : Integer range Result'Range := Result'First;

      Temp   : Interfaces.Unsigned_8;

      use Interfaces;
      use Interfaces.C;
   begin
      for I in Given'Range loop -- For each word
         Temp := Given (I);
         for J in reverse 0 .. 2 - 1 loop
            Result (Where + J) := Hex (Integer (Temp and 16#F#) + 1);
            Temp := Interfaces.Shift_Right (Temp, 4);
         end loop;
         if I /= Given'Last then
            exit when Where + 2 >= Result'Last;
            Where := Where + 2;
         end if;
      end loop;
      return Result;
   end To_String;

   ---------------------------
   -- To_String (Reader_ID) --
   ---------------------------

   function To_String (Reader : in SCard.Reader_ID) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Reader);
   end To_String;

   ---------------------------------
   -- To_String (Card_States_Set) --
   ---------------------------------

   function To_String (States : in SCard.Card_States_Set) return String
   is
      use Ada.Strings.Unbounded;

      Str_States : Unbounded_String;
      Position   : SCard.VOCSP.Cursor := States.Data.First;
      State      : SCard.Card_State;
   begin
      while SCard.VOCSP.Has_Element (Position) loop
         State := SCard.VOCSP.Element (Position);
         Str_States := SCard.Card_State'Image (State) & " " & Str_States;
         SCard.VOCSP.Next (Position);
      end loop;
      return To_String (Str_States);
   end To_String;

   -----------------------------------
   -- To_String (Reader_States_Set) --
   -----------------------------------

   function To_String (States : in SCard.Reader_States_Set) return String
   is
      use Ada.Strings.Unbounded;

      Str_States : Unbounded_String;
      Position   : SCard.VORSP.Cursor := States.Data.First;
      State      : SCard.Reader_State;
   begin
      while SCard.VORSP.Has_Element (Position) loop
         State := SCard.VORSP.Element (Position);
         Str_States := SCard.Reader_State'Image (State) & " " & Str_States;
         SCard.VORSP.Next (Position);
      end loop;
      return To_String (Str_States);
   end To_String;

   ----------------------
   -- For_Every_Reader --
   ----------------------

   procedure For_Every_Reader
     (Readers : in SCard.Readers_List;
      Call    : in SCard.Callback)
   is
      use SCard.Readers_Vector;

      Position : Cursor := Readers.First;
      Reader   : SCard.Reader_ID;
   begin
      while Has_Element (Position) loop
         Reader := Element (Position);
         --  Perform action on specific reader.
         Call (Reader);
         Next (Position);
      end loop;
   end For_Every_Reader;

   --------------------
   -- Print_ReaderID --
   --------------------

   procedure Print_ReaderID (ID : in SCard.Reader_ID) is
   begin
      Ada.Text_IO.Put_Line (To_String (ID));
   end Print_ReaderID;

end PCSC.SCard.Utils;
