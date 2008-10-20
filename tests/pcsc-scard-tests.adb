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
   end Initialize;

   -----------------------------
   -- Test_Slice_Readerstring --
   -----------------------------

   procedure Test_Slice_Readerstring is
      use type Ada.Containers.Count_Type;
      use type VOIDP.Vector;

      --  Separation and Termination chars
      Sep     : Character := Ada.Characters.Latin_1.NUL;
      Term    : String := Ada.Characters.Latin_1.NUL
        & Ada.Characters.Latin_1.NUL;

      --  Input sources
      Source1 : String := "This is not a reader string";
      Source2 : String := "One Reader" & Term;
      Source3 : String := "First  Reader" & Sep & "_SecondReader_" & Sep
        & " Third _ Reader  II " & Term;

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

end PCSC.SCard.Tests;
