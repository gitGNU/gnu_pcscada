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

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with PCSC.Thin;

package PCSC.SCard is

   package IC renames Interfaces.C;

   type Context is limited private;
   --  This is the PC/SC-Context. This object is initialized by
   --  Establish_Context below and is used with all services.

   type SCard_Scope is
     (Scope_User,     --  Scope in user space.
      Scope_Terminal, --  Scope in terminal.
      Scope_System);  --  Scope in system.
   --  Possible scope for PC/SC-context.

   subtype Reader_ID is Unbounded_String;
   --  Reader friendly name.

   package Readers_Vector is new Ada.Containers.Vectors (Positive, Reader_ID);
   use Readers_Vector;
   --  Vector of readers.

   subtype Readers_List is Readers_Vector.Vector;
   --  Readers list returned by List_Readers().

   procedure Establish_Context
     (Context : in out SCard.Context;
      Scope   : in SCard_Scope);
   --  Establish PC/SC-context. Possible Scope values are defined in
   --  SCard_Scope type.

   procedure Release_Context (Context : in out SCard.Context);
   --  Release acquired SCard-context.

   function Is_Valid (Context : in SCard.Context) return Boolean;
   --  Verify that given SCard-Context is valid.

   function List_Readers (Context : in SCard.Context)
                          return Readers_List;
   --  Return list of all available readers for this PC/SC context.


   -----------------------
   --  Helper functions --
   -----------------------

   function To_String (Reader : Reader_ID) return String;
   --  Return string from Reader_ID.

private

   function Slice_Readerstring (To_Slice : in String)
                                return Readers_List;
   --  Slice reader string returned from thin binding and create vector of
   --  reader names. The string to slice has a format like:
   --  Reader A1\0Reader B1\0Reader C1\0\0
   --  \0 is used as separator, \0\0 as string termination.

   procedure SCard_Exception (Code : in Thin.Return_Code; Message : in String);
   pragma No_Return (SCard_Exception);
   --  Raise SCard exception if something goes wrong.

   type Context is limited record
      C_Context : aliased Thin.SCARDCONTEXT;
   end record;

end PCSC.SCard;
