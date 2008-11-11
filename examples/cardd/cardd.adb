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

with PCSC.Version;
with PCSC.SCard.Utils;

use PCSC;

procedure Cardd is
   package SCU renames SCard.Utils;

   pragma Linker_Options ("-lpcsclite");

   Context      : SCard.Context;
   Reader_IDs   : SCard.Reader_ID_Set;
   Reader_Table : SCard.Reader_Condition_Set;

   function Create_Condition (Reader : SCard.Reader_ID)
                              return SCard.Reader_Condition is
      New_Condition : SCard.Reader_Condition;
   begin
      New_Condition.Name := Reader;
      New_Condition.Current_State.Add (State => SCard.S_Reader_Unaware);
      return New_Condition;
   end Create_Condition;

begin
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** PCSC/Ada card daemon [version " &
                         PCSC.Version.Version_String & "] **");
   Ada.Text_IO.New_Line;

   --  Establish context

   SCU.Action_Info (Text => "Establishing context");
   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Wait for the first reader

   SCU.Action_Info (Text => "Waiting for first reader");
   SCard.Wait_For_Readers (Context => Context);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Create reader list and check status changes

   SCU.Action_Info (Text => "Creating readers list");
   Reader_IDs := SCard.List_Readers (Context => Context);
   SCU.Action_Result (Result => SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line ("> Readers found            : ");
   SCU.For_Every_Reader (Readers => Reader_IDs,
                         Call    => SCU.Print_ReaderID'Access);

   for R in Natural range Reader_IDs.First_Index .. Reader_IDs.Last_Index loop
      Reader_Table.Add (Status => Create_Condition
                        (Reader => Reader_IDs.Get (R)));
   end loop;

   --  Enter main loop: detect status changes

   Ada.Text_IO.Put_Line ("Starting main loop ...");
   loop
      SCard.Status_Change (Context    => Context,
                           Conditions => Reader_Table);
      --  Update states
--        for C in Natural range Reader_Table.First_Index ..
--          Reader_Table.Last_Index loop
--           Reader_Table.Get (Index => C).Current_State :=
--             Reader_Table.Get (Index => C).Event_State;
--        end loop;

   end loop;

   --  Release context

   SCU.Action_Info (Text => "Releasing context");
   SCard.Release_Context (Context => Context);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("DONE!");
   Ada.Text_IO.New_Line;

exception
   when others =>
      SCU.Action_Result (Result => "FAILED: " & SCard.Get_Return_Code);

      if SCard.Is_Valid (Context => Context) then
         SCard.Release_Context (Context => Context);
      end if;
end Cardd;
