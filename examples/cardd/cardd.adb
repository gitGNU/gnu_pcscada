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
with PCSC.SCard.Monitor;

use PCSC;

procedure Cardd is
   package SCU renames SCard.Utils;

   pragma Linker_Options ("-lpcsclite");

   Context : aliased SCard.Context;
begin
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** PCSC/Ada card daemon [version " &
                         PCSC.Version.Version_String & "] **");
   Ada.Text_IO.New_Line;

   --  Establish context

   begin
      SCU.Action_Info (Text => "Establishing context");
      SCard.Establish_Context (Context => Context,
                               Scope   => SCard.Scope_System);
      SCU.Action_Result (Result => SCard.Get_Return_Code);
   exception
      when SCard_Error =>
         SCU.Action_Result (Result => "FAILED: " & SCard.Get_Return_Code);
         return;
      when others =>
         SCU.Action_Result (Result => "Unknown error.");
         return;
   end;

   --  Start the monitoring task

   declare
      Monitor : SCard.Monitor.Reader_Monitor;
   begin
      SCU.Action_Info (Text => "Starting Reader Monitor");
      Monitor.Init (Context => Context'Unchecked_Access);
      Monitor.Start;
   end;

end Cardd;
