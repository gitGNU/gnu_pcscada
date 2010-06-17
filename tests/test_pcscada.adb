--
--  Copyright (c) 2008-2010,
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

with Ada.Text_IO;
with Ada.Strings.Fixed;

with PCSC.Version;
with PCSC.SCard.Utils;

use PCSC;

--  PCSC/Ada integration test procedure. To run these tests, you need to have
--  at least one reader and one smartcard ready.
--
--  To ease comparing test runs, this procedure is implemented in a manner
--  similar to the pcsc-lite 'testpcsc' test binary, which should be used as
--  test reference.
procedure Test_PCSCAda is

   pragma Linker_Options ("-lpcsclite");

   package SCU renames SCard.Utils;

   Context      : SCard.Context;
   Card         : SCard.Card;

   Readers      : SCard.Reader_ID_Set;

   Reader_Table : SCard.Reader_Condition_Set;
   Reader1      : SCard.Reader_Condition;

begin

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** PCSC/Ada Thick-Binding test program [version " &
                         PCSC.Version.Version_String & "] **");
   Ada.Text_IO.New_Line;

   --  Establish context

   SCU.Action_Info (Text => "Testing Establish_Context");
   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Test for valid context

   SCU.Action_Info (Text => "Testing Is_Valid");
   if not SCard.Is_Valid (Context => Context) then
      SCU.Action_Result (Result => "FAILED : " & SCard.Get_Return_Code);
   end if;
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Wait for first reader to appear

   Ada.Text_IO.Put_Line ("Testing Wait_For_Readers");
   SCU.Action_Info (Text => "Please connect a reader");
   SCard.Wait_For_Readers (Context => Context);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  List readers

   SCU.Action_Info (Text => "Testing List_Readers");
   Readers := SCard.List_Readers (Context => Context);
   SCU.Action_Result (Result => SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line ("> Readers found            : ");
   SCU.For_Every_Reader (Readers => Readers,
                         Call    => SCU.Print_ReaderID'Access);

   --  Use first reader for status change detection.

   Reader1.Name := Readers.First_Item;
   Reader1.Current_State.Add (State => SCard.S_Reader_Empty);
   Reader_Table.Add (Status => Reader1);

   --  Detect status changes

   SCU.Action_Info (Text => "Waiting for card insertion");
   SCard.Status_Change (Context    => Context,
                        Conditions => Reader_Table);
   SCU.Action_Result (Result => SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line
     (">> Reader Name             : " &
      SCU.To_String (Reader_Table.Get (Index => 1).Name));
   Ada.Text_IO.Put_Line
     (">> Reader states           : " &
      SCU.To_String (Reader_Table.Get (Index => 1).Event_State));
   Ada.Text_IO.Put_Line
     (">> Card ATR                : " &
      SCU.To_Hex_String (Reader_Table.Get (Index => 1).Card_ATR));

   --  Connect to first reader

   SCU.Action_Info (Text => "Testing Connect");
   SCard.Connect (Context => Context,
                  Card    => Card,
                  Reader  => Readers.First_Item,
                  Mode    => SCard.Share_Shared);
   SCU.Action_Result (Result => SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line (">> Card uses protocol      : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Begin transaction with first reader

   SCU.Action_Info (Text => "Testing Begin_Transaction");
   SCard.Begin_Transaction (Card => Card);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Test status

   declare
      Card_States : SCard.Card_States_Set;
      Card_Proto  : SCard.Proto := SCard.Proto_Undefined;
      Card_ATR    : SCard.ATR;
   begin
      SCU.Action_Info (Text => "Testing Status");
      SCard.Status (Card  => Card,
                    State => Card_States,
                    Proto => Card_Proto,
                    Atr   => Card_ATR);
      SCU.Action_Result (Result => SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">> ATR                     : " &
                            SCU.To_Hex_String (Given => Card_ATR));
      Ada.Text_IO.Put_Line (">> ATR Size                : " &
                            SCard.Size (Card_ATR));
      Ada.Text_IO.Put_Line (">> Protocol                : " &
                            SCard.Proto'Image (Card_Proto));
      Ada.Text_IO.Put_Line (">> Card states             : " &
                            SCU.To_String (Card_States));
   end;

   --  Send arbitrary APDU to card

   declare
      Recv_Buffer : SCard.Byte_Set (1 .. 10);
      Send_Buffer : constant SCard.Byte_Set :=
        (16#00#, 16#A4#, 16#00#, 16#00#, 16#02#, 16#3F#, 16#00#);
      Recv_Len    : Natural := 0;
      Recv_PCI    : SCard.IO_Request;
   begin
      SCU.Action_Info (Text => "Testing Transmit");
      SCard.Transmit (Card        => Card,
                      Send_Buffer => Send_Buffer,
                      Recv_Pci    => Recv_PCI,
                      Recv_Buffer => Recv_Buffer,
                      Recv_Len    => Recv_Len);
      SCU.Action_Result (Result => SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">> APDU (select file)      : " &
                            String (SCU.To_Hex_String (Given => Send_Buffer)));
      Ada.Text_IO.Put_Line (">> Response from card      : " &
        String (SCU.To_Hex_String (Given => Recv_Buffer,
                                   Len   => 2 * Integer (Recv_Len))));
   end;

   --  Test smart card control

   declare
      Recv_Buffer  : SCard.Byte_Set (1 .. 10);
      Send_Buffer  : constant SCard.Byte_Set :=
        (16#06#, 16#00#, 16#0A#, 16#01#, 16#01#, 16#10#, 16#00#);
      Recv_Len     : Natural := 0;
      Control_Code : constant Integer := 16#42000001#;
   begin
      SCU.Action_Info (Text => "Testing Control");
      SCard.Control (Card        => Card,
                     Code        => Control_Code,
                     Send_Buffer => Send_Buffer,
                     Recv_Buffer => Recv_Buffer,
                     Recv_Len    => Recv_Len);
      SCU.Action_Result (Result => SCard.Get_Return_Code);
   exception
      when SCard_Error =>
         --  This test is allowed to fail
         SCU.Action_Result (Result => "FAILED (don't PANIC): "
                       & SCard.Get_Return_Code);
   end;

   --  Test Get_Attribute

   SCU.Action_Info (Text => "Testing Get_Attribute");
   begin
      declare
         Attr_Vendor   : SCard.Byte_Set := SCard.Init_Attribute_Set
           (Card => Card,
            Attr => SCard.Attr_Vendor_Name);
         Attr_ATR      : SCard.Byte_Set := SCard.Init_Attribute_Set
           (Card => Card,
            Attr => SCard.Attr_ATR_String);
         Attr_Maxinput : SCard.Byte_Set := SCard.Init_Attribute_Set
           (Card => Card,
            Attr => SCard.Attr_Maxinput);

         use Ada.Strings.Fixed;
      begin
         SCard.Get_Attribute (Card        => Card,
                              Attr        => SCard.Attr_Vendor_Name,
                              Recv_Buffer => Attr_Vendor);
         SCU.Action_Result (Result => SCard.Get_Return_Code);
         Ada.Text_IO.Put_Line (">> Attr_Vendor_Name is     : "
                               & SCU.To_String (Given => Attr_Vendor));
         Ada.Text_IO.Put_Line (">> Attr_Vendor_Name size   : "
           & Trim (Source => Attr_Vendor'Last'Img,
                   Side   => Ada.Strings.Left));

         SCU.Action_Info (Text => "Testing Get_Attribute");
         SCard.Get_Attribute (Card        => Card,
                              Attr        => SCard.Attr_ATR_String,
                              Recv_Buffer => Attr_ATR);
         SCU.Action_Result (Result => SCard.Get_Return_Code);
         Ada.Text_IO.Put_Line (">> Attr_ATR_String is      : "
                               & SCU.To_Hex_String (Given => Attr_ATR));
         Ada.Text_IO.Put_Line (">> Attr_ATR_String size    : "
           & Trim (Source => Attr_ATR'Last'Img,
                   Side   => Ada.Strings.Left));

         SCU.Action_Info (Text => "Testing Get_Attribute");
         SCard.Get_Attribute (Card        => Card,
                              Attr        => SCard.Attr_Maxinput,
                              Recv_Buffer => Attr_Maxinput);
         SCU.Action_Result (Result => SCard.Get_Return_Code);
         Ada.Text_IO.Put_Line (">> Attr_Maxinput is        : "
           & Trim (Source => SCU.To_Long_Long_Integer
                   (Given => Attr_Maxinput)'Img,
                   Side   => Ada.Strings.Left));
      end;
   exception
      when SCard_Error =>
         --  Most likely this happens when GetAttribute feature is not
         --  supported by the ifd handler of the reader.
         SCU.Action_Result (Result => "FAILED (don't PANIC): " &
                       SCard.Get_Return_Code);
   end;

   --  End transaction with first reader

   SCU.Action_Info (Text => "Testing End_Transaction");
   SCard.End_Transaction (Card   => Card,
                          Action => SCard.Leave_Card);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Reconnect to first reader

   SCU.Action_Info (Text => "Testing Reconnect");
   SCard.Reconnect (Card   => Card,
                    Mode   => SCard.Share_Shared,
                    Action => SCard.Unpower_Card);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Disconnect from first reader

   SCU.Action_Info (Text => "Testing Disconnect");
   SCard.Disconnect (Card   => Card,
                     Action => SCard.Unpower_Card);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   --  Release context

   SCU.Action_Info (Text => "Testing Release_Context");
   SCard.Release_Context (Context => Context);
   SCU.Action_Result (Result => SCard.Get_Return_Code);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("PCSC/Ada test completed successfully!");
   Ada.Text_IO.New_Line;

exception
   when others =>
      SCU.Action_Result (Result => "FAILED: " & SCard.Get_Return_Code);

      if SCard.Is_Valid (Context => Context) then
         SCard.Release_Context (Context => Context);
      end if;

      raise;
end Test_PCSCAda;
