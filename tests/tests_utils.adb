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

--  Ada
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;

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
                                  Routine => Convert_Long_Long_Integer'Access,
                                  Name    => "Byte_Set to Long_Long_Integer");
   end Initialize;

   ------------------------------------------
   -- Convert_Long_Long_Integer (Byte_Set) --
   ------------------------------------------

   procedure Convert_Long_Long_Integer is
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

   end Convert_Long_Long_Integer;

end Tests_Utils;
