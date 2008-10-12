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

--  Ahven
with Ahven; use Ahven;

--  PCSC/Ada
with PCSC.SCard.Utils;

use PCSC;

package body Tests_Utils is

   package SCU renames SCard.Utils;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T    => T,
                Name => "Tests for PCSC/Ada SCard Utils");

      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_To_Long_Long_Integer'Access,
                                  Name    => "Byte_Set to Long_Long_Integer");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_Byte_Set_To_String'Access,
                                  Name    => "Byte_Set to String");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_RStates_Set_To_String'Access,
                                  Name    => "Reader_States_Set to String");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_CStates_Set_To_String'Access,
                                  Name    => "Card_States_Set to String");
   end Initialize;

   ------------------------------------------
   -- Convert_Long_Long_Integer (Byte_Set) --
   ------------------------------------------

   procedure Test_To_Long_Long_Integer is
      Set_Small   : SCard.Byte_Set (1 .. 2) :=
        (16#12#, 16#FF#);
      Set_Big     : SCard.Byte_Set (1 .. 4) :=
        (16#AA#, 16#0A#, 16#BA#, 16#12#);

      Result  : Long_Long_Integer;
   begin
      --  Big byte set
      Result := SCU.To_Long_Long_Integer (Given => Set_Big);
      Assert (Condition => Result = 314182314,
              Message   => "result is not 314182314");

      --  Small byte set
      Result := SCU.To_Long_Long_Integer (Given => Set_Small);
      Assert (Condition => Result = 65298,
              Message   => "result is not 65298");

      declare
         Set_Too_Big : SCard.Byte_Set (1 .. 8) :=
           (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#);
      begin
         --  Byte set 'Set_Too_Long' cannot be represented by
         --  Long_Long_Integer, this test should raise Number_Too_Big exception
         Result := SCU.To_Long_Long_Integer (Given => Set_Too_Big);
         Fail (Message => "No Number_Too_Big exception raised");
      exception
         when SCU.Number_Too_Big =>
            null;
      end;

   end Test_To_Long_Long_Integer;

   -----------------------------
   -- Test_Byte_Set_To_String --
   -----------------------------

   procedure Test_Byte_Set_To_String is
      Empty_Set : SCard.Byte_Set := SCard.Null_Byte_Set;
      Test_Set  : SCard.Byte_Set (1 .. 4) := (16#70#, 16#63#, 16#73#, 16#63#);
   begin
      Assert (Condition => SCU.To_String (Given => Empty_Set) = "0",
              Message   => "Returned string not '0'");
      Assert (Condition => SCU.To_String (Given => Test_Set) = "pcsc",
              Message   => "Returned string not 'pcsc'");
   end Test_Byte_Set_To_String;

   --------------------------------
   -- Test_RStates_Set_To_String --
   --------------------------------

   procedure Test_RStates_Set_To_String is
      Empty_RStates : SCard.Reader_States_Set;
      RStates       : SCard.Reader_States_Set;
   begin
      Assert (Condition => SCU.To_String (States => Empty_RStates) = "",
              Message   => "String incorrect");

      --  Fill RStates set

      RStates.Add (State => SCard.S_Reader_Unaware);
      RStates.Add (State => SCard.S_Reader_Atrmatch);

      Assert (Condition => SCU.To_String (States => RStates) =
                "S_READER_ATRMATCH S_READER_UNAWARE",
              Message   => "String incorrect");
   end Test_RStates_Set_To_String;

   --------------------------------
   -- Test_CStates_Set_To_String --
   --------------------------------

   procedure Test_CStates_Set_To_String is
      Empty_CStates : SCard.Card_States_Set;
      CStates       : SCard.Card_States_Set;
   begin
      Assert (Condition => SCU.To_String (States => Empty_CStates) = "",
              Message   => "String incorrect");

      --  Fill CStates set

      CStates.Add (State => SCard.S_Card_Swallowed);
      CStates.Add (State => SCard.S_Card_Powered);

      Assert (Condition => SCU.To_String (States => CStates) =
                "S_CARD_POWERED S_CARD_SWALLOWED",
              Message   => "String incorrect");
   end Test_CStates_Set_To_String;

end Tests_Utils;
