--
--  Copyright (c) 2008-2009,
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
--    SCard reader monitoring package. This package provides types and
--    functions / procedures to monitor card readers.  The abstract type
--    Observer can be extended to create new observers for specific reader
--    state changes. Observers can be registered to a Reader_Monitor task by
--    calling the Register() entry. A running Reader_Monitor task will inform
--    all registered observers when a change in reader state occurs.
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

   function Is_Interested
     (O      : Observer;
      States : Reader_States_Set)
      return Boolean;
   --  Function is used to check whether an observer is interested in one of
   --  the states in given set.

   procedure Notify
     (O         : Observer;
      Condition : Reader_Condition) is abstract;
   --  This procedure is called to inform an observer about status changes in
   --  reader states for all states an observer is interested in.

   task type Reader_Monitor is
      entry Init (Context : Context_Handle);
      --  Initialization entry for a reader monitor task
      entry Start;
      --  Start the reader monitor task
      entry Stop;
      --  Stop the reader monitor task
      entry Register (O : Observer_Class);
      --  Register a new observer to the reader monitor
   end Reader_Monitor;
   --  Reader monitoring control task

private

   task type Status_Peeker is
      entry Run (Peek_Context : Context_Handle);
      entry Stop;
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
      entry Insert (Observer : Observer_Class);
      --  Insert a new observer into set.
      procedure Notify_All (Condition : Reader_Condition);
      --  Notify all registered observers that a reader state has changed.
   private
      My_Set    : Vector_Of_Observer_Type;
      --  Vector of observers
      Notifying : Boolean := False;
      --  Barrier condition to indicate whether a notification is currently in
      --  progress.
   end Protected_Observer_Set;
   --  Protected observer set. Used to store observers which are registered by
   --  the client code by calling the Register() entry of the Reader_Monitor
   --  task type.

   Observer_Set : Protected_Observer_Set;
   --  Set of registered observers. Accessed by Reader_Monitor and
   --  Status_Peeker tasks.

   function Create_Condition
     (Reader : SCard.Reader_ID)
      return SCard.Reader_Condition;
   --  Create a new reader condition object from given reader ID.

   procedure Update_Reader_Table
     (Table : in out SCard.Reader_Condition_Set;
      IDs   :        SCard.Reader_ID_Set);
   --  Update given reader condition table with IDs in given reader ID set. New
   --  readers, which are not already in the condition set 'Table',
   --  are added by calling Create_Condition(), vanished IDs are removed from
   --  the reader condition set 'Table'.

end PCSC.SCard.Monitor;
