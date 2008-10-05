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

   -------------------------------
   --  To_Hex_String (Byte_Set) --
   -------------------------------

   function To_Hex_String
     (Given : in Byte_Set := Null_Byte_Set;
      Len   : in Positive)
      return String
   is
      use Interfaces;
      use Interfaces.C;

      Hex    : constant String := "0123456789ABCDEF";

      Result : String (1 .. Len) := (others => '0');
      Where  : Integer range Result'Range := Result'First;

      Temp   : Unsigned_8;

   begin
      for Index in Given'Range loop -- For each word
         Temp := Given (Index);
         for J in reverse 0 .. 2 - 1 loop
            Result (Where + J) := Hex (Integer (Temp and 16#F#) + 1);
            Temp := Shift_Right (Temp, 4);
         end loop;
         if Index /= Given'Last then
            exit when Where + 2 >= Result'Last;
            Where := Where + 2;
         end if;
      end loop;
      return Result;
   end To_Hex_String;

   -------------------------------
   --  To_Hex_String (Byte_Set) --
   -------------------------------

   function To_Hex_String (Given : in Byte_Set := Null_Byte_Set) return String
   is
   begin
      if Given = Null_Byte_Set then
         return "0";
      end if;

      return To_Hex_String (Given => Given,
                            Len   => 2 * Given'Last);
   end To_Hex_String;

   ---------------------------------
   --  To_Hex_String (Byte_Array) --
   ---------------------------------

   function To_Hex_String (Given : in Thin.Byte_Array; Len : in Positive)
                           return String
   is
   begin
      return To_Hex_String (Given => Byte_Set (Given), Len => Len);
   end To_Hex_String;

   --------------------------
   --  To_Hex_String (ATR) --
   --------------------------

   function To_Hex_String (Given : in ATR := Null_ATR) return String is
   begin
      if Given = Null_ATR then
         return "0";
      end if;

      return To_Hex_String (Given => Byte_Set (Given.Data),
                            Len   => 2 * Positive (Given.Length));
   end To_Hex_String;

   ---------------------------
   -- To_String (Reader_ID) --
   ---------------------------

   function To_String (Reader : in Reader_ID := Null_Reader_ID)
                       return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Reader);
   end To_String;

   ---------------------------------
   -- To_String (Card_States_Set) --
   ---------------------------------

   function To_String (States : in Card_States_Set) return String
   is
      use Ada.Strings.Unbounded;

      Str_States : Unbounded_String;
      Position   : VOCSP.Cursor := States.Data.First;
      State      : Card_State;
   begin
      while VOCSP.Has_Element (Position) loop
         State := VOCSP.Element (Position);
         Str_States := Card_State'Image (State) & " " & Str_States;
         VOCSP.Next (Position);
      end loop;
      return To_String (Str_States);
   end To_String;

   -----------------------------------
   -- To_String (Reader_States_Set) --
   -----------------------------------

   function To_String (States : in Reader_States_Set) return String
   is
      use Ada.Strings.Unbounded;

      Str_States : Unbounded_String;
      Position   : VORSP.Cursor := States.Data.First;
      State      : Reader_State;
   begin
      while VORSP.Has_Element (Position) loop
         State := VORSP.Element (Position);
         Str_States := Reader_State'Image (State) & " " & Str_States;
         VORSP.Next (Position);
      end loop;
      return To_String (Str_States);
   end To_String;

   --------------------------
   -- To_String (Byte_Set) --
   --------------------------

   function To_String (Given : in Byte_Set := Null_Byte_Set) return String is
      New_String : String (1 .. Given'Length - 1);
   begin
      if Given = Null_Byte_Set then
         return "0";
      end if;

      for C in 1 .. Given'Length - 1 loop
         New_String (C) := Character'Val (Given (C));
      end loop;

      return New_String;
   end To_String;

   -------------------------------------
   -- To_Long_Long_Integer (Byte_Set) --
   -------------------------------------

   function To_Long_Long_Integer (Given : in Byte_Set := Null_Byte_Set)
                                  return Long_Long_Integer
   is
      use Interfaces;

      Result : Unsigned_64 := 0;
      U      : Unsigned_64 := 0;
   begin
      for Index in Given'Range loop
         U := Unsigned_64 (Given (Index));
         Result := Result or Shift_Left (U, (Index - 1) * 8);
      end loop;

      return Long_Long_Integer (Result);
   exception
      when Constraint_Error =>
         raise Number_Too_Big;
   end To_Long_Long_Integer;

   ----------------------
   -- For_Every_Reader --
   ----------------------

   procedure For_Every_Reader
     (Readers : in Reader_ID_Set;
      Call    : in Callback)
   is
      use VOIDP;

      Position : Cursor := Readers.Data.First;
      Reader   : Reader_ID;
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

   procedure Print_ReaderID (ID : in Reader_ID) is
   begin
      Ada.Text_IO.Put_Line (To_String (ID));
   end Print_ReaderID;

end PCSC.SCard.Utils;
