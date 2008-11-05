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

   Context     : SCard.Context;
   Card        : SCard.Card;

   Readers     : SCard.Reader_ID_Set;

   Card_States : SCard.Card_States_Set;
   Card_Proto  : SCard.Proto := SCard.Proto_Undefined;
   Card_ATR    : SCard.ATR;
begin

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** PCSC/Ada PIN verfification example [version " &
                         PCSC.Version.Version_String & "] **");
   Ada.Text_IO.New_Line;

   --  Establish context

   SCU.Action_Info (Text => "Establishing context");
   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Test for valid context

   SCU.Action_Info (Text => "Verifying context");
   if not SCard.Is_Valid (Context => Context) then
      SCU.Action_Result (Result => "FAILED : " & SCard.Get_Return_Code);
   end if;
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Get reader list

   SCU.Action_Info (Text => "Asking for readers");
   Readers := SCard.List_Readers (Context => Context);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   if Readers.Empty then
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("No Readers found!");
      SCard.Release_Context (Context => Context);
      return;
   end if;

   Ada.Text_IO.Put_Line ("> Readers found            : ");
   SCU.For_Every_Reader (Readers => Readers,
                         Call    => SCU.Print_ReaderID'Access);

   --  Connect to first reader

   SCU.Action_Info (Text => "Connecting to first reader");
   SCard.Connect (Context => Context,
                  Card    => Card,
                  Reader  => Readers.First,
                  Mode    => SCard.Share_Direct);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Get card status, return if no card is present

   SCU.Action_Info (Text => "Testing card status");
   SCard.Status (Card  => Card,
                 State => Card_States,
                 Proto => Card_Proto,
                 Atr   => Card_ATR);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   if Card_States.Is_In (State => SCard.S_Card_Absent) then
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("No card inserted!");
      return;
   end if;

   --  Test if reader supports PIN verification

   declare
      Recv_Buffer : SCard.Byte_Set (1 .. 2);
      Supported   : Boolean := False;

   begin
      SCU.Action_Info (Text => "Testing for verify feature");
      SCard.SPE_Init (Card   => Card,
                      Result => Supported);
      if not Supported then
         SCU.Action_Result (Result => "Not supported by reader.");
         return;
      end if;
      SCU.Action_Result (Result => "Supported by reader.");

      --  Execute SPE operation
      SCU.Action_Info (Text => "Enter PIN on pinpad");
      SCard.SPE_Exec (Card   => Card,
                      Result => Recv_Buffer);
      SCU.Action_Result (Result => SCard.Get_Return_Code);

      --  Display return code
      SCU.Action_Info (Text => "Return code from reader");
      SCU.Action_Result (Result => SCU.To_Hex_String (Given => Recv_Buffer));
   end;

   --  Disconnect from first reader

   SCU.Action_Info (Text => "Disconnecting");
   SCard.Disconnect (Card   => Card,
                     Action => SCard.Unpower_Card);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

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

      raise;
end Pinpad;
