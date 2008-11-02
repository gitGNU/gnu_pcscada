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

--  Test SPE (secure PIN entry) PIN verification by using the readers pinpad.
--  First, the Supports_SPE() function of the thick binding is used to test if
--  the reader supporst this operation. If the reader does support SPE, the
--  user is asked to enter the smart card PIN at the card reader.
procedure Pinpad is
   package SCU renames SCard.Utils;

   pragma Linker_Options ("-lpcsclite");

   Context       : SCard.Context;
   Card          : SCard.Card;

   Readers       : SCard.Reader_ID_Set;

   procedure Print_Testinfo (Text   : in String);
   procedure Print_Result   (Result : in String);
   --  Forward declarations to make compiler happy.

   procedure Print_Testinfo (Text : in String) is
   begin
      Ada.Text_IO.Put (Item => Text);
      Ada.Text_IO.Set_Col (To => 28);
      Ada.Text_IO.Put (":");
   end Print_Testinfo;

   procedure Print_Result (Result : in String) is
   begin
      Ada.Text_IO.Set_Col (To => 30);
      Ada.Text_IO.Put_Line (Item => Result);
   end Print_Result;
begin

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** PCSC/Ada PIN verfification example [version " &
                         PCSC.Version.Version_String & "] **");
   Ada.Text_IO.New_Line;

   --  Establish context

   Print_Testinfo (Text => "Establishing context");
   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);
   Print_Result (Result => SCard.Get_Return_Code);

   --  Test for valid context

   Print_Testinfo (Text => "Verifying context");
   if not SCard.Is_Valid (Context => Context) then
      Print_Result (Result => "FAILED : " & SCard.Get_Return_Code);
   end if;
   Print_Result (Result => SCard.Get_Return_Code);

   --  Get reader list

   Print_Testinfo (Text => "Asking for readers");
   Readers := SCard.List_Readers (Context => Context);
   Print_Result (Result => SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line ("> Readers found            : ");
   SCU.For_Every_Reader (Readers => Readers,
                         Call    => SCU.Print_ReaderID'Access);

   --  Connect to first reader, even without inserted card

   Print_Testinfo (Text => "Connecting to first reader");
   SCard.Connect (Context => Context,
                  Card    => Card,
                  Reader  => Readers.First,
                  Mode    => SCard.Share_Shared);
   Print_Result (Result => SCard.Get_Return_Code);

   --  Test if reader supports PIN verification

   Print_Testinfo (Text => "Testing for verify feature");
   if not SCard.Supports_SPE (Card => Card) then
      Print_Result (Result => "Not supported by reader.");
   else
      Print_Result (Result => "Supported.");
   end if;

   --  Disconnect from first reader

   Print_Testinfo (Text => "Disconnecting");
   SCard.Disconnect (Card   => Card,
                     Action => SCard.Unpower_Card);
   Print_Result (Result => SCard.Get_Return_Code);

   --  Release context

   Print_Testinfo (Text => "Releasing context");
   SCard.Release_Context (Context => Context);
   Print_Result (Result => SCard.Get_Return_Code);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("DONE!");
   Ada.Text_IO.New_Line;

end Pinpad;
