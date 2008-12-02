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
--  <PURPOSE>
--    SCard reader monitoring package.
--  </PURPOSE>
--

package PCSC.SCard.Monitor is

   type Context_Handle is access all SCard.Context;
   --  Handle to Context object

   type Observer is abstract tagged record
      States : Reader_States_Set;
      --  Reader states this observer is interested in
   end record;
   --  Abstract reader observer type. Valid Reader_Monitor observers must
   --  extend this type.

   subtype Observer_Class is Observer'Class;
   --  Observer class subtype

   procedure Notify (O         : in Observer;
                     Condition : in Reader_Condition) is abstract;
   --  This procedure is called to inform an observer about status changes in
   --  reader states for all states this observer is interested in.

   function Is_Interested (O      : in Observer;
                           States : in Reader_States_Set) return Boolean;
   --  Function is used to check whether an observer is interested in one of
   --  the states given by 'States' set.


   task type Reader_Monitor is
      entry Init (Context : in Context_Handle);
      --  Initialization entry for a Reader_Monitor task
      entry Start;
      --  Start the Reader_Monitor task
      entry Stop;
      --  Stop the Reader_Monitor task
      entry Register (O : in Observer_Class);
      --  Register a new observer to the Reader_Monitor
   end Reader_Monitor;
   --  Reader monitoring control task

private

   Current_Context : Context_Handle;
   --  Handle to current SCard.Context in use

   Do_Cancel : Boolean := False;
   --  Flag to signal monitor shutdown

   task type Status_Peeker is
      entry Run;
   end Status_Peeker;
   --  Status_Peeker task type. An object of this type can be used to detect
   --  condition changes for all readers of a SCard.Context. Condition changes
   --  are detected by calling the SCard.Status_Change procedure.

   package Vector_Of_Observer_Package is new
     Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                        Element_Type => Observer_Class);

   package VOOBP renames Vector_Of_Observer_Package;
   subtype Vector_Of_Observer_Type is VOOBP.Vector;

   protected type Protected_Observer_Set is
      entry Insert (Observer : in Observer_Class);
      --  Insert a new observer into set.
      procedure Notify_All (Condition : in Reader_Condition);
      --  Notify all registered observers that a reader state has changed.
   private
      My_Set    : Vector_Of_Observer_Type;
      --  Vector of observers
      Notifying : Boolean := False;
      --  Barrier condition to indicate whether a notification is currently in
      --  progress.
   end Protected_Observer_Set;
   --  Protected type observer set. Used to store observers which are
   --  registered by the client code by calling the Register() entry of the
   --  Reader_Monitor task type.

   Observer_Set : Protected_Observer_Set;
   --  Set of registered observers.


   function Create_Condition
     (Reader : in SCard.Reader_ID)
      return SCard.Reader_Condition;
   --  Create a new Reader_Conditon object from Reader_ID given.

   procedure Update_Reader_Table
     (Table : in out SCard.Reader_Condition_Set;
      IDs   : in SCard.Reader_ID_Set);
   --  Update given reader condition table with Reader_ID_Set list of reader
   --  IDs. New readers, which are not already in the condition set 'Table',
   --  are added by calling Create_Condition(), vanished IDs are removed from
   --  the Reader_Condition_Set 'Table'.

end PCSC.SCard.Monitor;
