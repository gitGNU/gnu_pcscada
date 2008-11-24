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
--  <PURPOSE>
--    SCard reader monitoring package.
--  </PURPOSE>
--

package PCSC.SCard.Monitor is

   type Context_Handle is access all SCard.Context;
   --  Handle to Context object

   task type Reader_Monitor is
      entry Init (Context : in Context_Handle);
      entry Start;
      entry Stop;
   end Reader_Monitor;
   --  Reader monitoring control task

private

   Current_Context : Context_Handle;
   --  Handle to current SCard.Context in use

   Do_Cancel : Boolean := False;
   --  Flag to signal monitor shutdown

   task type Status_Observer is
      entry Run;
   end Status_Observer;
   --  Status_Observer task type. An object of this type will check for status
   --  changes on all known readers.

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
