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
   Context       : SCard.Context;
   Card          : SCard.Card;

   Readers       : SCard.Reader_ID_Set;

   Reader_Status : SCard.Reader_Status_Set;
   Reader1       : SCard.Reader_Status;

   package SCU renames SCard.Utils;

   pragma Linker_Options ("-lpcsclite");
begin

   --  Establish context

   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);

   --  List readers

   Readers := SCard.List_Readers (Context => Context);
   if Readers.Empty then
      Ada.Text_IO.Put_Line ("no readers found ... waiting ...");
      SCard.Wait_For_Readers (Context => Context);

      --  Re-read readers list

      Readers := SCard.List_Readers (Context => Context);
   else
      Ada.Text_IO.Put_Line ("found readers: ");
      SCU.For_Every_Reader (Readers => Readers,
                            Call    => SCU.Print_ReaderID'Access);

   end if;

   --  Use first reader for status change detection.

   Reader1.Name := Readers.First;
   Reader1.Current_State := SCard.State_Empty;
   Reader_Status.Add_Reader (Reader1);

   --  Detect status changes

   Ada.Text_IO.Put_Line ("status change detection ...");
   SCard.Status_Change (Context    => Context,
                        Status_Set => Reader_Status);

   Ada.Text_IO.Put
     (SCU.To_String (Reader_Status.Get_Status (Index => 1).Name) & " : ");

   Ada.Text_IO.Put_Line
     (SCU.To_String (Reader_Status.Get_Status (Index => 1).Event_State));

   Ada.Text_IO.Put_Line
     (SCU.To_String (Reader_Status.Get_Status (Index => 1).Card_ATR));

   --  Connect to first reader

   Ada.Text_IO.Put_Line ("connecting to " &
                         SCU.To_String (Readers.First) & " ...");
   SCard.Connect (Card     => Card,
                  Context  => Context,
                  Reader   => Readers.First,
                  Mode     => SCard.Share_Shared);
   Ada.Text_IO.Put_Line ("card uses : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Reconnect to first reader

   Ada.Text_IO.Put_Line ("reconnecting to " &
                         SCU.To_String (Readers.First) & " ...");
   SCard.Reconnect (Card   => Card,
                    Mode   => SCard.Share_Exclusive,
                    Action => SCard.Leave_Card);
   Ada.Text_IO.Put_Line ("card uses : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Begin transaction with first reader

   Ada.Text_IO.Put_Line ("start transaction with " &
                         SCU.To_String (Readers.First) & " ...");
   SCard.Begin_Transaction (Card => Card);

   declare
      Card_States    : SCard.Card_States_Set;
      Reader_Proto   : SCard.Proto := SCard.Proto_Undefined;
      Reader_ATR     : SCard.ATR;
   begin

      --  Get status of reader / card

      Ada.Text_IO.Put_Line ("status of  " & SCU.To_String
                              (Readers.First) & " ...");
      SCard.Status (Card    => Card,
                    State   => Card_States,
                    Proto   => Reader_Proto,
                    Atr     => Reader_ATR);
      Ada.Text_IO.Put_Line ("  ATR      : " &
                            SCU.To_String (Given => Reader_ATR));
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
                         SCU.To_String (Readers.First) & " ...");
   SCard.End_Transaction (Card   => Card,
                          Action => SCard.Leave_Card);

   --  Disconnect from first reader

   Ada.Text_IO.Put_Line ("disconnecting from " &
                         SCU.To_String (Readers.First) & " ...");
   SCard.Disconnect (Card   => Card,
                     Action => SCard.Leave_Card);

   --  Release context

   Ada.Text_IO.Put_Line ("releasing context ...");
   SCard.Release_Context (Context => Context);
end Thick_Tests;
