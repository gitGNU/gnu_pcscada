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

with Ada.Characters.Latin_1;

with Ahven; use Ahven;

with PCSC.SCard.Utils;
with PCSC.SCard.Conversion;

package body PCSC.SCard.Tests is

   package Convert renames PCSC.SCard.Conversion;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T    => T,
                Name => "Tests for PCSC/Ada SCard Ada <=> C Conversions");

      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_Slice_Readerstring'Access,
                                  Name    => "String to Reader_ID_Set");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_To_C_RStatus_Set'Access,
                                  Name    => "To_C for Reader_Status_Set");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_To_Chars_Ptr'Access,
                                  Name    => "Reader_ID to LPSTR");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_To_Ada_Proto'Access,
                                  Name    => "DWORD to Proto type");
      Framework.Add_Test_Routine (T       => T,
                                  Routine => Test_To_Card_States_Set'Access,
                                  Name    => "DWORD to Card_States_Set");
   end Initialize;

   -----------------------------
   -- Test_Slice_Readerstring --
   -----------------------------

   procedure Test_Slice_Readerstring is
      use type Ada.Containers.Count_Type;
      use type VOIDP.Vector;

      --  Separation and Termination chars
      Sep     : constant Character := Ada.Characters.Latin_1.NUL;
      Term    : constant String := Ada.Characters.Latin_1.NUL
        & Ada.Characters.Latin_1.NUL;

      --  Input sources
      Source1 : constant String := "This is not a reader string";
      Source2 : constant String := "One Reader" & Term;
      Source3 : constant String := "First  Reader" & Sep & "_SecondReader_"
        & Sep & " Third _ Reader  II " & Term;

      --  Resulting reader ID set
      Readers : Reader_ID_Set;
   begin

      --  Test slicing of an ordinary, invalid string
      Readers := Convert.Slice_Readerstring (To_Slice => Source1);
      Assert (Condition => Readers.Data = VOIDP.Empty_Vector,
              Message   => "Reader_ID_Set not empty");

      --  Test slicing of one reader
      Readers := Convert.Slice_Readerstring (To_Slice => Source2);

      Assert (Condition => Readers.Data.Length = 1,
              Message   => "Slicing failed");
      Assert (Condition => Readers.First = "One Reader",
              Message   => "Reader name does not match");

      --  Test slicing of multiple readers
      Readers := Convert.Slice_Readerstring (To_Slice => Source3);
      Assert (Condition => Readers.Data.Length = 3,
              Message   => "Slicing failed");
      Assert (Condition => Readers.First = "First  Reader",
              Message   => "Reader name does not match");
      Assert (Condition => Readers.Get (Index => 2) = "_SecondReader_",
              Message   => "Reader name does not match");
      Assert (Condition => Readers.Last = " Third _ Reader  II ",
              Message   => "Reader name does not match");
   end Test_Slice_Readerstring;

   ---------------------------
   -- Test_To_C_RStatus_Set --
   ---------------------------

   procedure Test_To_C_RStatus_Set is
      --  Empty set test
      Empty_Set : Reader_Status_Set;

      --  This should return an empty array
      E_Result  : Thin.READERSTATE_Array := Convert.To_C (States => Empty_Set);

      --  Construct a 'real' status set
      Real_Set  : Reader_Status_Set;

      Reader1   : Reader_Status;
      Reader2   : Reader_Status;

      Useless   : Reader_States_Set;
   begin
      --  Empty
      Assert (Condition => E_Result'Length = 0,
              Message   => "array not empty");

      --  Fill real set
      Reader1.Name := To_Reader_ID ("Reader I");
      Reader1.Current_State := S_Reader_Unavailable;
      Reader2.Name := To_Reader_ID ("Reader II");
      Reader2.Current_State := S_Reader_Unaware;

      --  Setting Event_State of Reader2 should be ignored by the conversion,
      --  defaults are used instead. Settting Event_State manually makes no
      --  sense since they are updated by calling the Status_Change procedure.
      Useless.Add (State => S_Reader_Ignore);
      Useless.Add (State => S_Reader_Exclusive);
      Reader2.Event_State := Useless;

      Reader2.Card_ATR := To_Atr (Bytes => Byte_Set'(16#12#, 16#23#));

      --  Add reader status to set set
      Real_Set.Add (Status => Reader1);
      Real_Set.Add (Status => Reader2);

      declare
         use Thin;
         use Interfaces.C;

         R_Result : Thin.READERSTATE_Array := Convert.To_C
           (States => Real_Set);
      begin
         --  Test resulting size of array
         Assert (Condition => R_Result'Length = 2,
                 Message   => "array size mismatch");

         --  Test individual values one by one

         --  szReader names
         Assert (Condition => String'(Strings.Value
                 (R_Result (R_Result'First).szReader)) = "Reader I",
                 Message   => "reader name mismatch");
         Assert (Condition => String'(Strings.Value
                 (R_Result (R_Result'Last).szReader)) = "Reader II",
                 Message   => "reader name mismatch");

         --  pvUserData must be null
         Assert (Condition => R_Result (R_Result'First).pvUserData = null,
                 Message   => "pvUserData not null");
         Assert (Condition => R_Result (R_Result'Last).pvUserData = null,
                 Message   => "pvUserData not null");

         --  dwCurrentState
         Assert (Condition => R_Result (R_Result'First).dwCurrentState =
                   SCARD_STATE_UNAVAILABLE,
                 Message   => "dwCurrentState incorrect");
         Assert (Condition => R_Result (R_Result'Last).dwCurrentState =
                   SCARD_STATE_UNAWARE,
                 Message   => "dwCurrentState incorrect");

         --  dwEventState must be set to default, even though we specified
         --  Reader_States_Set 'Useless' above.
         Assert (Condition => R_Result (R_Result'First).dwEventState = 0,
                 Message   => "dwEventState incorrect");
         Assert (Condition => R_Result (R_Result'Last).dwEventState = 0,
                 Message   => "dwEventState incorrect");

         --  cbAtr value must match Card_ATR.Data'Length
         Assert (Condition => R_Result (R_Result'First).cbAtr =
                   Reader1.Card_ATR.Data'Length,
                 Message   => "cbAtr size incorrect");
         Assert (Condition => R_Result (R_Result'Last).cbAtr =
                   Reader2.Card_ATR.Data'Length,
                 Message   => "cbAtr size incorrect");

         --  rgbAtr (Byte_Array) length must match Card_ATR.Data'Length
         Assert (Condition => R_Result (R_Result'First).rgbAtr'Length =
                   Reader1.Card_ATR.Data'Length,
                 Message   => "rgbAtr size incorrect");
         Assert (Condition => R_Result (R_Result'Last).rgbAtr'Length =
                   Reader2.Card_ATR.Data'Length,
                 Message   => "rgbAtr size incorrect");

         --  rgbAtr (Byte_Array) must match Card_ATR.Data for Reader2 and
         --  Null_ATR for Reader1
         Assert (Condition => ATR_Type (Byte_Set (R_Result
                 (R_Result'First).rgbAtr)) = Null_ATR.Data,
                 Message   => "ATR data mismatch");
         Assert (Condition => ATR_Type (Byte_Set (R_Result
                 (R_Result'Last).rgbAtr)) = Reader2.Card_ATR.Data,
                 Message   => "ATR data mismatch");

         --  Free memory after test
         Convert.Free (Name => R_Result);
      end;
   end Test_To_C_RStatus_Set;

   -----------------------
   -- Test_To_Chars_Ptr --
   -----------------------

   procedure Test_To_Chars_Ptr is
      use Interfaces.C;

      ID  : constant Reader_ID := To_Reader_ID (Name => "Reader I");
      Ptr : Strings.chars_ptr := Convert.To_Chars_Ptr (Reader => ID);
   begin
      Assert (Condition => String'(Strings.Value (Item => Ptr)) =
                Utils.To_String (Reader => ID),
              Message => "Reader name mismatch");

      --  Free memory
      Strings.Free (Item => Ptr);
   end Test_To_Chars_Ptr;

   -----------------------
   -- Test_To_Ada_Proto --
   -----------------------

   procedure Test_To_Ada_Proto is
      No_Proto : constant Thin.DWORD := 16#FFFF_FFFF#;

      RAW      : constant Thin.DWORD := Thin.SCARD_PROTOCOL_RAW;
   begin
      Assert (Condition => Convert.To_Ada (C_Protocol => No_Proto) =
                Proto_Undefined,
              Message   => "Proto not Proto_Undefined");

      --  Just test one Thin.DWORD to Proto conversion, no need to test all
      Assert (Condition => Convert.To_Ada (C_Protocol => RAW) = Proto_RAW,
              Message   => "Proto not Proto_RAW");
   end Test_To_Ada_Proto;

   -----------------------------
   -- Test_To_Card_States_Set --
   -----------------------------

   procedure Test_To_Card_States_Set is
      use VOCSP;
      use type Interfaces.C.unsigned_long;
      use type Ada.Containers.Count_Type;

      No_CSet  : constant Thin.DWORD := 0;
      S3_Set   : constant Thin.DWORD := Thin.SCARD_UNKNOWN + Thin.SCARD_ABSENT
        + Thin.SCARD_SWALLOWED;
      Real_Set : constant Card_States_Set := Convert.To_Ada
        (C_Cardstate => S3_Set);
   begin
      Assert (Condition => Convert.To_Ada (C_Cardstate => No_CSet).Data =
                VOCSP.Empty_Vector,
              Message   => "CStates_Set not empty");

      --  Test converted Real_Set
      Assert (Condition => Real_Set.Data.Length = 3,
              Message   => "Expected 3 Card_States");

      Assert (Condition => Real_Set.Data.Find (Item => S_Card_Unknown) /=
                VOCSP.No_Element,
              Message   => "Card_Powered not found");
      Assert (Condition => Real_Set.Data.Find (Item => S_Card_Absent) /=
                VOCSP.No_Element,
              Message   => "Card_Absent not found");
      Assert (Condition => Real_Set.Data.Find (Item => S_Card_Swallowed) /=
                VOCSP.No_Element,
              Message   => "Card_Swallowed not found");
   end Test_To_Card_States_Set;

end PCSC.SCard.Tests;
