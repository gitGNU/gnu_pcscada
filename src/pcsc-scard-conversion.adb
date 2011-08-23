--
--  Copyright (c) 2008-2009,
--  Reto Buerki <reet@codelabs.ch>
--
--  This file is part of PCSC/Ada.
--
--  PCSC/Ada is free software; you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Publ License as published
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

with Ada.Strings.Maps;
with Ada.Characters.Latin_1;

with GNAT.String_Split;

package body PCSC.SCard.Conversion is

   use Interfaces.C;

   -------------------------------------------------------------------------

   procedure Free (Name : in out Thin.READERSTATE_Array) is
   begin
      for Index in Name'Range loop
         Strings.Free (Name (Index).szReader);
      end loop;
   end Free;

   -------------------------------------------------------------------------

   function Slice_Readerstring (To_Slice : String) return Reader_ID_Set
   is
      use type GNAT.String_Split.Slice_Number;

      Readers : Reader_ID_Set;
      Lines   : GNAT.String_Split.Slice_Set;
   begin
      --  Slice readers into parts.
      --  Who uses '\0' as separator anyway?

      GNAT.String_Split.Create
        (S          => Lines,
         From       => To_Slice (To_Slice'First .. To_Slice'Last),
         Separators => Ada.Strings.Maps.To_Set (Ada.Characters.Latin_1.NUL),
         Mode       => GNAT.String_Split.Single);

      --  Minus two because \0\0 is used as termination.

      for J in 1 .. GNAT.String_Split.Slice_Count (Lines) - 2 loop
         Readers.Data.Append (New_Item => To_Unbounded_String
                              (GNAT.String_Split.Slice (Lines, J)));
      end loop;

      return Readers;

   end Slice_Readerstring;

   -------------------------------------------------------------------------

   function To_Ada (C_Protocol : Thin.DWORD) return Proto is
   begin
      for P in Proto'Range loop
         if C_Proto (P) = C_Protocol then

            --  Return active Proto

            return P;
         end if;
      end loop;

      --  Return 'Undefined' if no active proto found

      return Proto_Undefined;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (C_Cardstate : Thin.DWORD) return Card_States_Set is
      States : Card_States_Set;
   begin
      for P in C_Card_State'Range loop
         if (C_Cardstate and C_Card_State (P)) /= 0 then
            States.Data.Append (New_Item => P);
         end if;
      end loop;
      return States;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (C_Readerstate : Thin.DWORD) return Reader_States_Set is
      States : Reader_States_Set;
   begin
      for P in C_Reader_State'Range loop
         if (C_Readerstate and C_Reader_State (P)) /= 0 then
            States.Data.Append (New_Item => P);
         end if;
      end loop;
      return States;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_C
     (Conditions : Reader_Condition_Set)
      return Thin.READERSTATE_Array
   is
      use VORCP;

      Position : Cursor := Conditions.Data.First;
      C_States : Thin.READERSTATE_Array
        (size_t (1) .. size_t (Conditions.Data.Last_Index));

      function Create_Readerstate
        (Condition : Reader_Condition;
         State     : Thin.DWORD)
         return Thin.READERSTATE;
      --  Helper function to create a new Thin.READERSTATE object from
      --  Reader_Condition

      function Create_Readerstate
        (Condition : Reader_Condition;
         State     : Thin.DWORD)
         return Thin.READERSTATE
      is
         Current_State : Thin.DWORD := State;
         Temp          : Interfaces.Unsigned_64;
      begin

         --  Add event counter in the upper word of dwCurrentState

         Current_State := State and 16#FFFF#;
         Temp          := Interfaces.Unsigned_64 (Condition.Event_Counter);
         Current_State := Current_State or Thin.DWORD
           (Interfaces.Shift_Left (Value  => Temp,
                                   Amount => 16));

         return Thin.READERSTATE'
           (szReader       => Strings.New_String
              (To_String (Condition.Name)),
            dwCurrentState => Current_State,
            cbAtr          => Condition.Card_ATR.Data'Length,
            rgbAtr         => Thin.Byte_Array (Condition.Card_ATR.Data),
            others         => <>);
      end Create_Readerstate;

   begin
      while Has_Element (Position) loop
         declare
            Item     : constant Reader_Condition := Element (Position);
            C_RState : Thin.DWORD                := 0;
         begin
            for S in Natural range Item.Current_State.First_Index ..
              Item.Current_State.Last_Index loop
               C_RState := C_RState or C_Reader_State
                 (Item.Current_State.Get (Index => S));
            end loop;

            C_States (size_t (To_Index (Position))) := Create_Readerstate
              (Condition => Item,
               State     => C_RState);

            Next (Position);
         end;
      end loop;

      return C_States;
   end To_C;

   -------------------------------------------------------------------------

   function To_Chars_Ptr (Reader : Reader_ID) return Strings.chars_ptr is
   begin
      return Strings.New_String (To_String (Reader));
   end To_Chars_Ptr;

end PCSC.SCard.Conversion;
