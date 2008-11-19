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
      Reader_IDnew : SCard.Reader_ID_Set;
      Reader_Table : SCard.Reader_Condition_Set;

      function Create_Condition (Reader : in SCard.Reader_ID)
                                 return SCard.Reader_Condition
      is
         New_Condition : SCard.Reader_Condition;
      begin
         New_Condition.Name := Reader;
         New_Condition.Current_State.Add (State => SCard.S_Reader_Unaware);
         return New_Condition;
      end Create_Condition;

      procedure Update_Reader_Table
        (Table : in out SCard.Reader_Condition_Set;
         IDs   : in SCard.Reader_ID_Set)
      is
         use type VOIDP.Cursor;

         Position : VORCP.Cursor := Table.Data.First;
         Item     : Reader_Condition;
      begin
         --  Remove vanished readers

         while VORCP.Has_Element (Position) loop
            Item := VORCP.Element (Position);
            if IDs.Data.Find (Item.Name) = VOIDP.No_Element then
               Ada.Text_IO.Put_Line ("Removing reader " &
                                     Utils.To_String (Reader => Item.Name));
               Table.Data.Delete (Position);
            end if;
            VORCP.Next (Position);
         end loop;

         --  Add new readers to table

         for R in Natural range IDs.First_Index .. IDs.Last_Index
         loop
            --  Skip already known readers

            if not Table.Find (Reader_ID => IDs.Get (R)) then
               Ada.Text_IO.Put_Line ("Adding reader " &
                                     Utils.To_String (Reader => IDs.Get (R)));
               Table.Add (Status => Create_Condition
                          (Reader => IDs.Get (R)));
            end if;
         end loop;

      end Update_Reader_Table;

   begin

      --  Enter main loop: detect status changes

      Ada.Text_IO.Put_Line ("Monitoring thread running ...");
      loop
         SCard.Status_Change (Context    => Context,
                              Conditions => Reader_Table);

         --  Check for new readers; if new ones are found, add them to the
         --  Reader_Table. If none found, an exception is thrown

         begin
            Reader_IDnew := SCard.List_Readers (Context => Context);
            if Reader_IDnew /= Reader_IDs then
               Update_Reader_Table (Table => Reader_Table,
                                    IDs   => Reader_IDnew);
               Reader_IDs := Reader_IDnew;
            end if;
         exception
            when SCard_Error =>

               --  No readers present, set empty vectors

               Ada.Text_IO.Put_Line ("All readers vanished ... ");
               Reader_Table.Data := VORCP.Empty_Vector;
               Reader_IDs.Data   := VOIDP.Empty_Vector;
         end;

         --  Loop through reader table and check for state S_Reader_Changed.
         --  If Event_State contains S_Reader_Changed, update Current_State
         --  with Event_State reader states.

         declare
            Position : VORCP.Cursor := Reader_Table.Data.First;
            Item     : Reader_Condition;
         begin
            while VORCP.Has_Element (Position) loop
               Item := VORCP.Element (Position);
               if Item.Event_State.Is_In (State => SCard.S_Reader_Changed) then
                  Item.Event_State.Remove (State => SCard.S_Reader_Changed);
                  Item.Current_State := Item.Event_State;
                  Reader_Table.Data.Replace_Element (Position => Position,
                                                     New_Item => Item);

                  --  Dump new reader states

                  Ada.Text_IO.Put_Line (Utils.To_String (Item.Name) & " : " &
                                        Utils.To_String (Item.Current_State));
               end if;
               VORCP.Next (Position);
            end loop;
         end;
      end loop;
   end Run;

end PCSC.SCard.Monitor;
