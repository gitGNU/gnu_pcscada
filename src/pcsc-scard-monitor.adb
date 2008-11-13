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

with PCSC.SCard.Utils;

package body PCSC.SCard.Monitor is

   ---------
   -- Run --
   ---------

   procedure Run (Context : in SCard.Context) is

      Reader_IDs   : SCard.Reader_ID_Set;
      Reader_Table : SCard.Reader_Condition_Set;
      First_Run    : Boolean := True;

      function Create_Condition (Reader : SCard.Reader_ID)
                                 return SCard.Reader_Condition
      is
         New_Condition : SCard.Reader_Condition;
      begin
         New_Condition.Name := Reader;
         New_Condition.Current_State.Add (State => SCard.S_Reader_Unaware);
         return New_Condition;
      end Create_Condition;

   begin
      --  Wait for the first reader

      SCard.Wait_For_Readers (Context => Context);

      --  Create reader table

      Reader_IDs := SCard.List_Readers (Context => Context);
      for R in Natural range Reader_IDs.First_Index .. Reader_IDs.Last_Index
      loop
         Reader_Table.Add (Status => Create_Condition
                           (Reader => Reader_IDs.Get (R)));
      end loop;

      --  Enter main loop: detect status changes

      Ada.Text_IO.Put_Line ("Monitoring thread running ...");
      loop
         SCard.Status_Change (Context    => Context,
                              Conditions => Reader_Table);

         --  TODO: check for new readers
         --        if new ones -> add to Reader_Table

         --  Loop through reader table and check for state S_Reader_Changed.
         --  If Event_State contains S_Reader_Changed, update Current_State
         --  with Event_State reader states.

         declare
            Position : VORCP.Cursor := Reader_Table.Data.First;
            Item     : Reader_Condition;
         begin
            while VORCP.Has_Element (Position) loop
               Item := VORCP.Element (Position);
               if Item.Event_State.Is_In
                 (State => SCard.S_Reader_Changed) and not First_Run then
                  Item.Current_State := Item.Event_State;
                  Reader_Table.Data.Replace_Element (Position => Position,
                                                     New_Item => Item);

                  --  Dump new states for all changed states

                  Ada.Text_IO.Put_Line (Utils.To_String (Item.Name) & " : " &
                                        Utils.To_String (Item.Current_State));
               end if;
               VORCP.Next (Position);
            end loop;
         end;
         First_Run := False;
      end loop;
   end Run;

end PCSC.SCard.Monitor;
