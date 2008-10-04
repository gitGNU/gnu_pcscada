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
with Ada.Strings.Fixed;

with PCSC.SCard;
with PCSC.SCard.Utils;

use PCSC;

--  PCSC/Ada integration test procedure. To run these tests, you need to have
--  at least one reader and one smartcard ready.
--
--  To ease comparing test runs, this procedure is implemented in a manner
--  similar to the pcsc-lite 'testpcsc' test binary, which should be used as
--  test reference.
procedure Test_PCSCAda is
   Context       : SCard.Context;
   Card          : SCard.Card;

   Readers       : SCard.Reader_ID_Set;

   Reader_Status : SCard.Reader_Status_Set;
   Reader1       : SCard.Reader_Status;

   package SCU renames SCard.Utils;

   pragma Linker_Options ("-lpcsclite");

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
   Ada.Text_IO.Put_Line ("** PCSC/Ada Thick-Binding test program **");
   Ada.Text_IO.New_Line;

   --  Establish context

   Print_Testinfo (Text => "Testing Establish_Context");
   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);
   Print_Result (Result => SCard.Get_Return_Code);

   --  Test for valid context

   Print_Testinfo (Text => "Testing Is_Valid");
   if not SCard.Is_Valid (Context => Context) then
      Print_Result (Result => "FAILED : " & SCard.Get_Return_Code);
   end if;
   Print_Result (Result => SCard.Get_Return_Code);

   --  Status change detection

   Ada.Text_IO.Put_Line ("Testing Wait_For_Readers");
   Print_Testinfo (Text => "Please connect a reader");
   SCard.Wait_For_Readers (Context => Context);
   Print_Result (Result => SCard.Get_Return_Code);

   --  List readers

   Print_Testinfo (Text => "Testing List_Readers");
   Readers := SCard.List_Readers (Context => Context);
   Print_Result (Result => SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line ("> Readers found            : ");
   SCU.For_Every_Reader (Readers => Readers,
                         Call    => SCU.Print_ReaderID'Access);

   --  Use first reader for status change detection.

   Reader1.Name := Readers.First;
   Reader1.Current_State := SCard.State_Empty;
   Reader_Status.Add_Reader (Reader1);

   --  Detect status changes

   Print_Testinfo (Text => "Waiting for card insertion");
   SCard.Status_Change (Context    => Context,
                        Status_Set => Reader_Status);
   Print_Result (Result => SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line
     (">> Reader Name             : " &
      SCU.To_String (Reader_Status.Get_Status (Index => 1).Name));
   Ada.Text_IO.Put_Line
     (">> Card Status             : " &
      SCU.To_String (Reader_Status.Get_Status (Index => 1).Event_State));
   Ada.Text_IO.Put_Line
     (">> Card ATR                : " &
      SCU.To_Hex_String (Reader_Status.Get_Status (Index => 1).Card_ATR));

   --  Connect to first reader

   Print_Testinfo (Text => "Testing Connect");
   SCard.Connect (Context  => Context,
                  Card     => Card,
                  Reader   => Readers.First,
                  Mode     => SCard.Share_Shared);
   Print_Result (Result => SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line (">> Card uses protocol      : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Begin transaction with first reader

   Print_Testinfo (Text => "Testing Begin_Transaction");
   SCard.Begin_Transaction (Card => Card);
   Print_Result (Result => SCard.Get_Return_Code);

   --  Test status

   declare
      Card_States    : SCard.Card_States_Set;
      Reader_Proto   : SCard.Proto := SCard.Proto_Undefined;
      Reader_ATR     : SCard.ATR;
   begin
      Print_Testinfo (Text => "Testing Status");
      SCard.Status (Card    => Card,
                    State   => Card_States,
                    Proto   => Reader_Proto,
                    Atr     => Reader_ATR);
      Print_Result (Result => SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">>  ATR                    : " &
                            SCU.To_Hex_String (Given => Reader_ATR));
      Ada.Text_IO.Put_Line (">>  ATR Size               : " &
                            SCard.Size (Reader_ATR));
      Ada.Text_IO.Put_Line (">>  Protocol               : " &
                            SCard.Proto'Image (Reader_Proto));
      Ada.Text_IO.Put_Line (">>  States                 : " &
                            SCU.To_String (Card_States));
   end;

   --  Send arbitrary APDU to card

   declare
      Recv_Buffer : SCard.Byte_Set (1 .. 10);
      Send_Buffer : SCard.Byte_Set :=
        (16#00#, 16#A4#, 16#00#, 16#00#, 16#02#, 16#3F#, 16#00#);
      Recv_Len    : Natural := 0;
   begin
      Print_Testinfo (Text => "Testing Transmit");
      SCard.Transmit (Card        => Card,
                      Send_Pci    => SCard.PCI_T1,
                      Send_Buffer => Send_Buffer,
                      Recv_Pci    => SCard.PCI_T1,
                      Recv_Buffer => Recv_Buffer,
                      Recv_Len    => Recv_Len);
      Print_Result (Result => SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">> APDU (select file)      : " &
                            String (SCU.To_Hex_String (Given => Send_Buffer)));
      Ada.Text_IO.Put_Line (">> Response from card      : " &
        String (SCU.To_Hex_String (Given => Recv_Buffer,
                                   Len   => 2 * Integer (Recv_Len))));
   end;

   --  Test smart card control

   declare
      Recv_Buffer  : SCard.Byte_Set (1 .. 10);
      Send_Buffer  : SCard.Byte_Set :=
        (16#06#, 16#00#, 16#0A#, 16#01#, 16#01#, 16#10#, 16#00#);
      Recv_Len     : Natural := 0;
      Control_Code : Integer := 16#42000001#;
   begin
      Print_Testinfo (Text => "Testing Control");
      SCard.Control (Card        => Card,
                     Code        => Control_Code,
                     Send_Buffer => Send_Buffer,
                     Recv_Buffer => Recv_Buffer,
                     Recv_Len    => Recv_Len);
      Print_Result (Result => SCard.Get_Return_Code);
   exception
      when others =>
         --  This is allowed to fail
         Print_Result (Result => "FAILED (don't PANIC)");
   end;

   --  Test Get_Attribute

   Print_Testinfo (Text => "Testing Get_Attribute");
   declare
      Attr_Vendor      : SCard.Byte_Set := SCard.Init_Attribute_Set
        (Card => Card, Attr => SCard.Attr_Vendor_Name);
      Attr_IFD_Version : SCard.Byte_Set := SCard.Init_Attribute_Set
        (Card => Card, Attr => SCard.Attr_Vendor_IFD_Version);

      Attr_ATR         : SCard.Byte_Set := SCard.Init_Attribute_Set
        (Card => Card, Attr => SCard.Attr_ATR_String);
      Attr_Maxinput    : SCard.Byte_Set := SCard.Init_Attribute_Set
        (Card => Card, Attr => SCard.Attr_Maxinput);

      use Ada.Strings.Fixed;
   begin
      SCard.Get_Attribute (Card        => Card,
                           Attr        => SCard.Attr_Vendor_Name,
                           Recv_Buffer => Attr_Vendor);
      Print_Result (Result => SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">> Attr_Vendor_Name is     : "
                            & SCU.To_String (Given => Attr_Vendor));
      Ada.Text_IO.Put_Line (">> Attr_Vendor_Name size   : "
        & Trim (Integer'Image (Attr_Vendor'Last), Ada.Strings.Left));

      Print_Testinfo (Text => "Testing Get_Attribute");
      SCard.Get_Attribute (Card        => Card,
                           Attr        => SCard.Attr_ATR_String,
                           Recv_Buffer => Attr_ATR);
      Print_Result (Result => SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">> Attr_ATR_String is      : "
        & SCU.To_Hex_String (Given => Attr_ATR));
      Ada.Text_IO.Put_Line (">> Attr_ATR_String size    : "
        & Trim (Integer'Image (Attr_ATR'Last), Ada.Strings.Left));

      Print_Testinfo (Text => "Testing Get_Attribute");
      SCard.Get_Attribute (Card        => Card,
                           Attr        => SCard.Attr_Maxinput,
                           Recv_Buffer => Attr_Maxinput);
      Print_Result (Result => SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">> Attr_Maxinput is        : "
        & Trim (Long_Long_Integer'Image (SCU.To_Long_Long_Integer
          (Given => Attr_Maxinput)), Ada.Strings.Left));
   end;

   --  End transaction with first reader

   Print_Testinfo (Text => "Testing End_Transaction");
   SCard.End_Transaction (Card   => Card,
                          Action => SCard.Leave_Card);
   Print_Result (Result => SCard.Get_Return_Code);

   --  Reconnect to first reader

   Print_Testinfo (Text => "Testing Reconnect");
   SCard.Reconnect (Card   => Card,
                    Mode   => SCard.Share_Shared,
                    Action => SCard.Unpower_Card);
   Print_Result (Result => SCard.Get_Return_Code);

   --  Disconnect from first reader

   Print_Testinfo (Text => "Testing Disconnect");
   SCard.Disconnect (Card   => Card,
                     Action => SCard.Unpower_Card);
   Print_Result (Result => SCard.Get_Return_Code);

   --  Release context

   Print_Testinfo (Text => "Testing Release_Context");
   SCard.Release_Context (Context => Context);
   Print_Result (Result => SCard.Get_Return_Code);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("PCSC/Ada test completed successfully!");
   Ada.Text_IO.New_Line;

exception
   when others =>
      Print_Result (Result => "FAILED: " & SCard.Get_Return_Code);

      if SCard.Is_Valid (Context => Context) then
         SCard.Release_Context (Context => Context);
      end if;

      raise;
end Test_PCSCAda;
