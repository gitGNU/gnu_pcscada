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

--  TODO: remove again!!
with PCSC.Thin;

--  Thick-binding test
procedure Thick_Tests is
   Context       : SCard.Context;
   Card          : SCard.Card;

   Readers       : SCard.Reader_ID_Set;

   Reader_Status : SCard.Reader_Status_Set;
   Reader1       : SCard.Reader_Status;

   package SCU renames SCard.Utils;

   pragma Linker_Options ("-lpcsclite");
begin

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** PCSC/Ada Thick-Binding test program **");
   Ada.Text_IO.New_Line;

   --  Establish context

   Ada.Text_IO.Put ("Testing Establish_Context : ");
   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);

   --  Test for valid context

   Ada.Text_IO.Put ("Testing Is_Valid          : ");
   if not SCard.Is_Valid (Context => Context) then
      Ada.Text_IO.Put_Line ("FAILED : " & SCard.Get_Return_Code);
   end if;
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);

   --  Status change detection

   Ada.Text_IO.Put_Line ("Testing Wait_For_Readers");
   Ada.Text_IO.Put ("Please connect a reader   : ");
   SCard.Wait_For_Readers (Context => Context);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);

   --  List readers

   Ada.Text_IO.Put ("Testing List_Readers      : ");
   Readers := SCard.List_Readers (Context => Context);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line ("> Readers found           : ");
   SCU.For_Every_Reader (Readers => Readers,
                         Call    => SCU.Print_ReaderID'Access);

   --  Use first reader for status change detection.

   Reader1.Name := Readers.First;
   Reader1.Current_State := SCard.State_Empty;
   Reader_Status.Add_Reader (Reader1);

   --  Detect status changes

   Ada.Text_IO.Put ("Waiting for card insertion: ");
   SCard.Status_Change (Context    => Context,
                        Status_Set => Reader_Status);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line
     (">> Reader Name            : " &
      SCU.To_String (Reader_Status.Get_Status (Index => 1).Name));
   Ada.Text_IO.Put_Line
     (">> Card Status            : " &
      SCU.To_String (Reader_Status.Get_Status (Index => 1).Event_State));
   Ada.Text_IO.Put_Line
     (">> Card ATR               : " &
      SCU.To_String (Reader_Status.Get_Status (Index => 1).Card_ATR));

   --  Connect to first reader

   Ada.Text_IO.Put ("Testing Connect           : ");
   SCard.Connect (Card     => Card,
                  Context  => Context,
                  Reader   => Readers.First,
                  Mode     => SCard.Share_Shared);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);
   Ada.Text_IO.Put_Line (">> Card uses protocol     : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Begin transaction with first reader

   Ada.Text_IO.Put ("Testing Begin_Transaction : ");
   SCard.Begin_Transaction (Card => Card);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);

   --  Test status

   declare
      Card_States    : SCard.Card_States_Set;
      Reader_Proto   : SCard.Proto := SCard.Proto_Undefined;
      Reader_ATR     : SCard.ATR;
   begin
      Ada.Text_IO.Put ("Testing Status            : ");
      SCard.Status (Card    => Card,
                    State   => Card_States,
                    Proto   => Reader_Proto,
                    Atr     => Reader_ATR);
      Ada.Text_IO.Put_Line (SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">>  ATR                   : " &
                            SCU.To_String (Given => Reader_ATR));
      Ada.Text_IO.Put_Line (">>  ATR Size              : " &
                            SCard.Size (Reader_ATR));
      Ada.Text_IO.Put_Line (">>  Protocol              : " &
                            SCard.Proto'Image (Reader_Proto));
      Ada.Text_IO.Put_Line (">>  States                : " &
                            SCU.To_String (Card_States));
   end;

   --  Send arbitrary APDU to card

   declare
      Recv_Buffer : SCard.Byte_Set (1 .. 10);
      Send_Buffer : SCard.Byte_Set :=
        (16#00#, 16#A4#, 16#00#, 16#00#, 16#02#, 16#3F#, 16#00#);
      Recv_Len    : Natural := 0;
   begin
      Ada.Text_IO.Put ("Testing Transmit          : ");
      SCard.Transmit (Card        => Card,
                      Send_Pci    => SCard.PCI_T1,
                      Send_Buffer => Send_Buffer,
                      Recv_Pci    => SCard.PCI_T1,
                      Recv_Buffer => Recv_Buffer,
                      Recv_Len    => Recv_Len);
      Ada.Text_IO.Put_Line (SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">> APDU (select file)     : " &
        String (SCU.To_String (Given => Send_Buffer,
                               Len   => 2 * Integer (Send_Buffer'Last))));
      Ada.Text_IO.Put_Line (">> Response from card     : " &
        String (SCU.To_String (Given => Recv_Buffer,
                               Len   => 2 * Integer (Recv_Len))));
   end;

   --  Test Get_Attribute

   declare
      Buffer : SCard.Byte_Set := SCard.Init_Attribute_Set
        (Card => Card, Attr => SCard.Attr_Vendor_Name);

      use Ada.Strings.Fixed;
   begin
      Ada.Text_IO.Put ("Testing Get_Attribute     : ");
      SCard.Get_Attribute (Card        => Card,
                           Attr        => SCard.Attr_Vendor_Name,
                           Recv_Buffer => Buffer);
      Ada.Text_IO.Put_Line (SCard.Get_Return_Code);
      Ada.Text_IO.Put_Line (">> Attr_Vendor_Name is    : "
                            & SCU.To_String (Given => Buffer));
      Ada.Text_IO.Put_Line (">> Attr_Vendor_Name size  : "
        & Trim (Integer'Image (Buffer'Last), Ada.Strings.Left));
   end;

   --  End transaction with first reader

   Ada.Text_IO.Put ("Testing End_Transaction   : ");
   SCard.End_Transaction (Card   => Card,
                          Action => SCard.Leave_Card);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);

   --  Reconnect to first reader

   Ada.Text_IO.Put ("Testing Reconnect         : ");
   SCard.Reconnect (Card   => Card,
                    Mode   => SCard.Share_Shared,
                    Action => SCard.Unpower_Card);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);

   --  Disconnect from first reader

   Ada.Text_IO.Put ("Testing Disconnect        : ");
   SCard.Disconnect (Card   => Card,
                     Action => SCard.Unpower_Card);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);

   --  Release context

   Ada.Text_IO.Put ("Testing Release_Context   : ");
   SCard.Release_Context (Context => Context);
   Ada.Text_IO.Put_Line (SCard.Get_Return_Code);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("PCSC/Ada test completed successfully!");
   Ada.Text_IO.New_Line;

exception
   when others =>
      Ada.Text_IO.Put_Line ("FAILED: " & SCard.Get_Return_Code);

      if SCard.Is_Valid (Context => Context) then
         SCard.Release_Context (Context => Context);
      end if;

      raise;
end Thick_Tests;
