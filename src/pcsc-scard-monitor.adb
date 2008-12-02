--
--  Copyright (c) 2008,
--  Reto Buerki <reet@codelabs.ch>
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

package body PCSC.SCard.Monitor is

   -------------------
   -- Is_Interested --
   -------------------

   function Is_Interested (O      : in Observer;
                           States : in Reader_States_Set) return Boolean
   is
      use type VORSP.Cursor;

      Position : VORSP.Cursor := O.States.Data.First;
   begin
      while VORSP.Has_Element (Position) loop
         if States.Data.Find (Item => VORSP.Element (Position))
           /= VORSP.No_Element then
            return True;
         end if;
         VORSP.Next (Position);
      end loop;
      return False;
   end Is_Interested;

   --------------------
   -- Reader_Monitor --
   --------------------

   task body Reader_Monitor is
      Current_Context : Context_Handle;
      --  Handle to current SCard.Context in use
      Peeker          : Status_Peeker;
      --  Status peeker task
      Stop_Monitor    : Boolean := False;
      --  Flag to signal monitor shutdown
   begin
      accept Init (Context : in Context_Handle) do
         Current_Context := Context;
      end Init;

      loop
         exit when Stop_Monitor;
         select
            accept Start;

            --  Start status peeker task

            Peeker.Run (Peek_Context => Current_Context);
         or
            when not Stop_Monitor =>
               accept Stop do
                  Stop_Monitor := True;
                  if SCard.Is_Valid (Context => Current_Context.all) then
                     SCard.Cancel (Context => Current_Context.all);
                  end if;
               end Stop;

               --  Stop status peeker task

               Peeker.Stop;
         or
            when not Stop_Monitor =>
               accept Register (O : in Observer_Class) do
                  Observer_Set.Insert (Observer => O);
               end Register;
         end select;
      end loop;
   end Reader_Monitor;

   -------------------
   -- Status_Peeker --
   -------------------

   task body Status_Peeker is
      Reader_IDs      : SCard.Reader_ID_Set;
      Reader_IDnew    : SCard.Reader_ID_Set;
      Reader_Table    : SCard.Reader_Condition_Set;
      Current_Context : Context_Handle;
      Stop_Peeker     : Boolean := False;
      --  Flag to signal peeker shutdown
   begin
      loop
         exit when Stop_Peeker;
         select
            accept Run (Peek_Context : Context_Handle) do
               Current_Context := Peek_Context;
            end Run;

            --  Main status detection loop

            loop
               SCard.Status_Change (Context    => Current_Context.all,
                                    Conditions => Reader_Table);

               --  Check for new readers; if new ones are found, add them to
               --  the Reader_Table. If none found, an exception is thrown.

               begin
                  Reader_IDnew := SCard.List_Readers
                    (Context => Current_Context.all);

                  if Reader_IDnew /= Reader_IDs then
                     Update_Reader_Table (Table => Reader_Table,
                                          IDs   => Reader_IDnew);
                     Reader_IDs := Reader_IDnew;
                  end if;

               exception
                  when SCard_Error =>

                     --  No readers present, set empty vectors
                     --  TODO: add possibility to notify this event

                     Reader_Table.Data := VORCP.Empty_Vector;
                     Reader_IDs.Data   := VOIDP.Empty_Vector;
               end;

               --  Loop through reader table and check for state
               --  S_Reader_Changed

               declare
                  Position : VORCP.Cursor := Reader_Table.Data.First;
                  Item     : Reader_Condition;
               begin
                  while VORCP.Has_Element (Position) loop
                     Item := VORCP.Element (Position);
                     if Item.Event_State.Is_In
                       (State => SCard.S_Reader_Changed) then

                        --  Event_State contains S_Reader_Changed, update
                        --  Current_State with new Event_State.

                        Item.Event_State.Remove
                          (State => SCard.S_Reader_Changed);
                        Item.Current_State := Item.Event_State;
                        Reader_Table.Data.Replace_Element
                          (Position => Position,
                           New_Item => Item);

                        --  Notify all interested observers

                        Observer_Set.Notify_All (Condition => Item);
                     end if;
                     VORCP.Next (Position);
                  end loop;
               end;
            end loop;
         or
            when not Stop_Peeker =>
               accept Stop do
                  Stop_Peeker := True;
               end Stop;
         end select;
      end loop;
   end Status_Peeker;

   ----------------------------
   -- Protected_Observer_Set --
   ----------------------------

   protected body Protected_Observer_Set is
      entry Insert (Observer : in Observer_Class) when not Notifying is
      begin
         My_Set.Append (New_Item => Observer);
      end Insert;

      procedure Notify_All (Condition : in Reader_Condition) is
         Position : VOOBP.Cursor := My_Set.First;
      begin
         Notifying := True;

         while VOOBP.Has_Element (Position) loop
            if VOOBP.Element (Position).Is_Interested
              (States => Condition.Current_State) then
               VOOBP.Element (Position).Notify (Condition);
            end if;
            VOOBP.Next (Position);
         end loop;

         Notifying := False;
      end Notify_All;
   end Protected_Observer_Set;

   ----------------------
   -- Create_Condition --
   ----------------------

   function Create_Condition
     (Reader : in SCard.Reader_ID)
      return SCard.Reader_Condition
   is
      New_Condition : SCard.Reader_Condition;
   begin
      New_Condition.Name := Reader;
      New_Condition.Current_State.Add (State => SCard.S_Reader_Unaware);
      return New_Condition;
   end Create_Condition;

   -------------------------
   -- Update_Reader_Table --
   -------------------------

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
            --  TODO: add possibility to notify a reader vanished event
            Table.Data.Delete (Position);
         end if;
         VORCP.Next (Position);
      end loop;

      --  Add new readers to table

      for R in Natural range IDs.First_Index .. IDs.Last_Index
      loop

         --  Skip already known readers

         if not Table.Find (Reader_ID => IDs.Get (R)) then
            --  TODO: add possibility to notify a new reader event
            Table.Add (Status => Create_Condition
                       (Reader => IDs.Get (R)));
         end if;
      end loop;
   end Update_Reader_Table;

end PCSC.SCard.Monitor;
