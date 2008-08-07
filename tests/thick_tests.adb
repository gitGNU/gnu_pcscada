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

with PCSC.SCard;
with PCSC.SCard.Utils;

use PCSC;

--  Thick-binding test
procedure Thick_Tests is
   Context : SCard.Context;
   Readers : SCard.Readers_List;
   Card    : SCard.Card;

   package SCU renames SCard.Utils;

   pragma Linker_Options ("-lpcsclite");
begin

   --  Establish context

   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);

   --  List readers

   Readers := SCard.List_Readers (Context => Context);
   SCU.For_Every_Reader (Readers => Readers,
                         Call    => SCU.Print_ReaderID'Access);

   --  Connect to first reader

   Ada.Text_IO.Put_Line ("connecting to " &
                         SCU.To_String (Readers.First_Element) & " ...");
   SCard.Connect (Card     => Card,
                  Context  => Context,
                  Reader   => Readers.First_Element,
                  Mode     => SCard.Share_Shared);
   Ada.Text_IO.Put_Line ("card uses : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Reconnect to first reader

   Ada.Text_IO.Put_Line ("reconnecting to " &
                         SCU.To_String (Readers.First_Element) & " ...");
   SCard.Reconnect (Card   => Card,
                    Mode   => SCard.Share_Exclusive,
                    Action => SCard.Leave_Card);
   Ada.Text_IO.Put_Line ("card uses : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Begin transaction with first reader

   Ada.Text_IO.Put_Line ("start transaction with " &
                         SCU.To_String (Readers.First_Element) & " ...");
   SCard.Begin_Transaction (Card => Card);

   declare
      Card_States    : SCard.Card_States;
      Reader_Proto   : SCard.Proto := SCard.Proto_Undefined;
      Reader_ATR     : SCard.ATR;
      Reader_ATR_Len : Integer := SCard.ATR_Length;
   begin

      --  Get status of reader / card

      Ada.Text_IO.Put_Line ("status of  " & SCU.To_String
                              (Readers.First_Element) & " ...");
      SCard.Status (Card    => Card,
                    State   => Card_States,
                    Proto   => Reader_Proto,
                    Atr     => Reader_ATR,
                    Atr_Len => Reader_ATR_Len);
      Ada.Text_IO.Put_Line ("  ATR      : " & SCU.To_String
                            (Given => Reader_ATR, Len => 2 * Reader_ATR_Len));
      Ada.Text_IO.Put_Line ("  protocol : " &
                            SCard.Proto'Image (Reader_Proto));
      Ada.Text_IO.Put_Line ("  states   : " &
                            SCU.To_String (Card_States));
   end;

   --  Send arbitrary APDU to card
   declare
      Recv_Buffer : SCard.Byte_Set (1 .. 10);
      Send_Buffer : SCard.Byte_Set :=
        (16#00#, 16#A4#, 16#00#, 16#00#, 16#02#, 16#3F#, 16#00#);
      Recv_Len    : Natural := 0;
   begin
      Ada.Text_IO.Put_Line ("sending APDU: " &
        String (SCU.To_String (Given => Send_Buffer,
                               Len   => 2 * Integer (Send_Buffer'Last))));

      SCard.Transmit (Card        => Card,
                      Send_Pci    => SCard.PCI_T1,
                      Send_Buffer => Send_Buffer,
                      Recv_Pci    => SCard.PCI_T1,
                      Recv_Buffer => Recv_Buffer,
                      Recv_Len    => Recv_Len);
      Ada.Text_IO.Put_Line ("response from card: " &
        String (SCU.To_String (Given => Recv_Buffer,
                               Len   => 2 * Integer (Recv_Len))));
   end;

   --  End transaction with first reader

   Ada.Text_IO.Put_Line ("ending transaction with " &
                         SCU.To_String (Readers.First_Element) & " ...");
   SCard.End_Transaction (Card   => Card,
                          Action => SCard.Leave_Card);

   --  Disconnect from first reader

   Ada.Text_IO.Put_Line ("disconnecting from " &
                         SCU.To_String (Readers.First_Element) & " ...");
   SCard.Disconnect (Card   => Card,
                     Action => SCard.Leave_Card);

   --  Release context

   Ada.Text_IO.Put_Line ("releasing context ...");
   SCard.Release_Context (Context => Context);
end Thick_Tests;
