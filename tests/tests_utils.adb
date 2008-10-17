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
with PCSC.Thin;
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
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_ReaderID_To_String'Access,
                                  Name    => "Reader_ID to String");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_ATR_To_Hex_String'Access,
                                  Name    => "ATR to HEX String");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_BArray_To_Hex_String'Access,
                                  Name    => "Byte_Array to HEX String");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_BSet_To_Hex_String'Access,
                                  Name    => "Byte_Set to HEX String");
   end Initialize;

   ---------------------------------------
   -- Test_Long_Long_Integer (Byte_Set) --
   ---------------------------------------

   procedure Test_To_Long_Long_Integer is
      Null_Set    : SCard.Byte_Set := SCard.Null_Byte_Set;

      Small_Set   : SCard.Byte_Set (1 .. 2) :=
        (16#12#, 16#FF#);
      Big_Set     : SCard.Byte_Set (1 .. 4) :=
        (16#AA#, 16#0A#, 16#BA#, 16#12#);

      Result  : Long_Long_Integer;
   begin
      --  Null_Byte_Set
      Result := SCU.To_Long_Long_Integer (Given => Null_Set);
      Assert (Condition => Result = 0,
              Message   => "result is not 0");

      --  Big byte set
      Result := SCU.To_Long_Long_Integer (Given => Big_Set);
      Assert (Condition => Result = 314182314,
              Message   => "result is not 314182314");

      --  Small byte set
      Result := SCU.To_Long_Long_Integer (Given => Small_Set);
      Assert (Condition => Result = 65298,
              Message   => "result is not 65298");

      declare
         Set_Too_Big : SCard.Byte_Set (1 .. 8) :=
           (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#);
      begin
         --  Byte set 'Set_Too_Big' cannot be represented by
         --  Long_Long_Integer, this test should raise Number_Too_Big exception
         Result := SCU.To_Long_Long_Integer (Given => Set_Too_Big);
         Fail (Message => "No Number_Too_Big exception raised");
      exception
         when Bytes_Too_Big =>
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

      Assert (Condition => SCU.To_String
              (States => RStates) = "S_READER_ATRMATCH S_READER_UNAWARE",
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

      Assert (Condition => SCU.To_String
              (States => CStates) = "S_CARD_POWERED S_CARD_SWALLOWED",
              Message   => "String incorrect");
   end Test_CStates_Set_To_String;

   -----------------------------
   -- Test_ReaderID_To_String --
   -----------------------------

   procedure Test_ReaderID_To_String is
      Null_Reader : SCard.Reader_ID := SCard.Null_Reader_ID;
   begin
      Assert (Condition => SCU.To_String (Reader => Null_Reader) = "",
              Message   => "String incorrect");
   end Test_ReaderID_To_String;

   ----------------------------
   -- Test_ATR_To_Hex_String --
   ----------------------------

   procedure Test_ATR_To_Hex_String is
      Null_ATR      : SCard.ATR := SCard.Null_ATR;

      ATR_Bytes     : SCard.Byte_Set := (16#2C#, 16#23#, 16#AB#, 16#8B#);
      Reader_ATR    : SCard.ATR := SCard.To_Atr (Bytes => ATR_Bytes);
   begin
      Assert (Condition => SCU.To_Hex_String (Given => Null_ATR) = "0",
              Message   => "Hex string incorrect");

      Assert (Condition => SCU.To_Hex_String
              (Given => Reader_ATR) = "2C23AB8B",
              Message   => "Hex string incorrect");
   end Test_ATR_To_Hex_String;

   -------------------------------
   -- Test_BArray_To_Hex_String --
   -------------------------------

   procedure Test_BArray_To_Hex_String is
      Empty_Array : Thin.Byte_Array := Thin.Null_Byte_Array;
      Real_Array  : Thin.Byte_Array := (16#2C#, 16#FF#, 16#78#, 16#AF#);
   begin
      Assert (Condition => SCU.To_Hex_String (Given => Empty_Array,
                                              Len   => 1234) = "0",
              Message   => "Hex string incorrect");
      Assert (Condition => SCU.To_Hex_String
              (Given => Real_Array,
               Len   => 2 * Real_Array'Length) = "2CFF78AF",
              Message   => "Hex string incorrect");
   end Test_BArray_To_Hex_String;

   -----------------------------
   -- Test_BSet_To_Hex_String --
   -----------------------------

   procedure Test_BSet_To_Hex_String is
      Empty_Set : SCard.Byte_Set := SCard.Null_Byte_Set;
      Real_Set  : SCard.Byte_Set :=
        (16#00#, 16#A4#, 16#00#, 16#00#, 16#02#, 16#3F#, 16#00#);
   begin
      Assert (Condition => SCU.To_Hex_String (Given => Empty_Set) = "0",
              Message   => "Hex string incorrect");

      Assert (Condition => SCU.To_Hex_String
              (Given => Real_Set) = "00A40000023F00",
              Message   => "Hex string incorrect");
   end Test_BSet_To_Hex_String;

end Tests_Utils;
