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
with Ada.Containers.Vectors;

with PCSC.SCard;
with PCSC.Utils;

use PCSC;

--  Thick-binding test.
procedure Runner is
   Context : SCard.Context;
   Readers : SCard.Readers_List;
   Card    : SCard.Card;

   pragma Linker_Options ("-lpcsclite");

   procedure Print_ReaderID (ID : in SCard.Reader_ID);

   procedure Print_ReaderID (ID : in SCard.Reader_ID) is
   begin
      Ada.Text_IO.Put_Line (Utils.To_String (ID));
   end Print_ReaderID;

begin

   --  Establish context

   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);

   --  List readers

   Readers := SCard.List_Readers (Context => Context);
   Utils.For_Every_Reader (Readers => Readers,
                           Call    => Print_ReaderID'Unrestricted_Access);

   --  Connect to first reader

   Ada.Text_IO.Put_Line ("Connecting to " &
                         Utils.To_String (Readers.First_Element) & " ...");
   SCard.Connect (Card     => Card,
                  Context  => Context,
                  Reader   => Readers.First_Element,
                  Mode     => SCard.Share_Shared);
   Ada.Text_IO.Put_Line ("Card uses : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Reconnect to first reader

   Ada.Text_IO.Put_Line ("Reconnecting to " &
                         Utils.To_String (Readers.First_Element) & " ...");
   SCard.Reconnect (Card   => Card,
                    Mode   => SCard.Share_Exclusive,
                    Action => SCard.Leave_Card);
   Ada.Text_IO.Put_Line ("Card uses : " & SCard.Proto'Image
                         (SCard.Get_Active_Proto (Card => Card)));

   --  Begin transaction with first reader

   Ada.Text_IO.Put_Line ("Beginning transaction with " &
                         Utils.To_String (Readers.First_Element) & " ...");
   SCard.Begin_Transaction (Card => Card);

   declare
      Card_States    : SCard.Card_State_Array (1 .. 3);
      Reader_Proto   : SCard.Proto := SCard.Proto_Undefined;
      Reader_ATR     : SCard.ATR;
      Reader_ATR_Len : Integer := SCard.ATR_Length;
   begin

      --  Get status of reader / card

      Ada.Text_IO.Put_Line ("Asking for status of  " &
                            Utils.To_String (Readers.First_Element) & " ...");
      SCard.Status (Card    => Card,
                    State   => Card_States,
                    Proto   => Reader_Proto,
                    Atr     => Reader_ATR,
                    Atr_Len => Reader_ATR_Len);
      Ada.Text_IO.Put_Line ("  ATR      : " & Utils.To_String
                            (Given => Reader_ATR, Len => 2 * Reader_ATR_Len));
      Ada.Text_IO.Put_Line ("  Protocol : " &
                            SCard.Proto'Image (Reader_Proto));
      Ada.Text_IO.Put_Line ("  States   : " &
                            Utils.To_String (Card_States));
   end;

   --  End transaction with first reader

   Ada.Text_IO.Put_Line ("Ending transaction with " &
                         Utils.To_String (Readers.First_Element) & " ...");
   SCard.End_Transaction (Card   => Card,
                          Action => SCard.Leave_Card);

   --  Disconnect from first reader

   Ada.Text_IO.Put_Line ("Disconnecting from " &
                         Utils.To_String (Readers.First_Element) & " ...");
   SCard.Disconnect (Card   => Card,
                     Action => SCard.Leave_Card);

   --  Release context

   Ada.Text_IO.Put_Line ("Releasing context ...");
   SCard.Release_Context (Context => Context);

   Ada.Text_IO.Put_Line ("DONE");

end Runner;

----  Thin-binding test.
--  procedure Runner is
--     hContext : aliased SCARDCONTEXT;
--     ret      : DWORD;
--
--     use Interfaces.C;
--
--     pragma Linker_Options ("-lpcsclite");
--  begin
--
--     ret := SCardEstablishContext
--       (dwScope     => SCARD_SCOPE_SYSTEM,
--        phContext   => hContext'Unchecked_Access);
--     if ret = SCARD_S_SUCCESS then
--        Ada.Text_IO.Put_Line ("context established");
--     end if;
--
--     ret := SCardIsValidContext (hContext => hContext);
--     if ret = SCARD_S_SUCCESS then
--        Ada.Text_IO.Put_Line ("context is valid");
--     end if;
--
--     declare
--        dwReaders        : aliased DWORD;
--        mszReaders       : LPSTR;
--        hCard            : aliased SCARDHANDLE;
--        dwActiveProtocol : aliased DWORD;
--
--        dwState          : aliased DWORD;
--        dwProtocol       : aliased DWORD;
--        dwAtrLen         : aliased DWORD := MAX_ATR_SIZE;
--        dwReaderLen      : aliased DWORD;
--
--        pbAtr            : Byte_Array (1 .. MAX_ATR_SIZE);
--
--        String_Code      : LPSTR;
--
--        Reader_Name      : C.Strings.chars_ptr := C.Strings.New_String
--          ("KOBIL KAAN SIM III (K_000000000) 00 00");
--     begin
--
--        --  List readers.
--
--        ret := SCardListReaders (hContext    => hContext,
--                                 mszReaders  =>
--                                   Interfaces.C.Strings.Null_Ptr,
--                                 pcchReaders => dwReaders'Access);
--
--        mszReaders := C.Strings.To_Chars_Ptr
--          (new C.char_array (C.size_t (1) .. C.size_t (dwReaders)));
--
--        ret := SCardListReaders (hContext    => hContext,
--                                 mszReaders  => mszReaders,
--                                 pcchReaders => dwReaders'Access);
--        Ada.Text_IO.Put_Line ("readers: " &
--                              Interfaces.C.Strings.Value (mszReaders));
--
--        declare
--           rgbAtr : ATR   := Null_ATR;
--           rgReaderStates : aliased SCARD_READERSTATE;
--
--        begin
--           --  Status change detection.
--           rgReaderStates.szReader := Reader_Name;
--           ret := SCardGetStatusChange
--             (hContext       => hContext,
--              dwTimeout      => INFINITE,
--              rgReaderStates => rgReaderStates'Unchecked_Access,
--              cReaders       => 1);
--
--           if ret = SCARD_S_SUCCESS then
--              Ada.Text_IO.Put_Line ("status change detected");
--              Ada.Text_IO.Put_Line ("status of reader is (dec): " &
--                                    Integer'Image
--                                      (Integer (
--                                         rgReaderStates.dwEventState)));
--           end if;
--        end;
--
--        --  Try to connect.
--
--        ret := SCardConnect (hContext             => hContext,
--                             szReader             => Reader_Name,
--                             dwShareMode          => SCARD_SHARE_SHARED,
--                             dwPreferredProtocols => SCARD_PROTOCOL_T0 or
--                                                     SCARD_PROTOCOL_T1,
--                             phCard               => hCard'Access,
--                             pdwActiveProtocol    =>
--                                                    dwActiveProtocol'Access);
--
--        if ret = SCARD_S_SUCCESS then
--           Ada.Text_IO.Put_Line ("connect ok");
--
--           --  Try to re-connect.
--           ret := SCardReconnect (hCard                => hCard,
--                                  dwShareMode          => SCARD_SHARE_SHARED,
--                                  dwPreferredProtocols =>
--                                                     SCARD_PROTOCOL_T0 or
--                                                          SCARD_PROTOCOL_T1,
--                                  dwInitialization     => SCARD_LEAVE_CARD,
--                                  pdwActiveProtocol    =>
--                                    dwActiveProtocol'Access);
--           if ret = SCARD_S_SUCCESS then
--              Ada.Text_IO.Put_Line ("re-connect ok");
--           end if;
--        end if;
--
--        --  Get status.
--        ret := SCardStatus (hCard          => hCard,
--                            mszReaderNames => C.Strings.Null_Ptr,
--                            pcchReaderLen  => dwReaderLen'Access,
--                            pdwState       => dwState'Unchecked_Access,
--                            pdwProtocol    => dwProtocol'Access,
--                            pbAtr          =>
--                              pbAtr (pbAtr'First)'Unchecked_Access,
--                            pcbAtrLen      => dwAtrLen'Access);
--
--        if ret = SCARD_S_SUCCESS then
--           Ada.Text_IO.Put_Line ("card status ok");
--           Ada.Text_IO.Put_Line ("  -- ATR len: " & DWORD'Image (dwAtrLen));
--           Ada.Text_IO.Put_Line ("  -- ATR    :  " &
--                                 String (To_String (Given => pbAtr,
--                                                    Len   => 2 * Integer
--                                                      (dwAtrLen))));
--           Ada.Text_IO.Put_Line ("  -- STATE  : " & DWORD'Image (dwState));
--           Ada.Text_IO.Put_Line ("  -- PROTO  : " &
--                                 DWORD'Image (dwProtocol));
--        end if;
--
--        --  Begin transaction.
--        ret := SCardBeginTransaction (hCard => hCard);
--        if ret = SCARD_S_SUCCESS then
--           Ada.Text_IO.Put_Line ("transaction start ok");
--        end if;
--
--        declare
--           pbRecvBuffer  : Byte_Array (1 .. 10);
--           pbSendBuffer  : Byte_Array :=
--             (16#00#, 16#A4#, 16#00#, 16#00#, 16#02#, 16#3F#, 16#00#);
--           pcbRecvLength : aliased DWORD := 10;
--        begin
--
--           --  Send arbitrary APDU to card.
--           ret := SCardTransmit (hCard         => hCard,
--                                 pioSendPci    => SCARD_PCI_T1'Access,
--                                 pbSendBuffer  => pbSendBuffer
--                                   (pbSendBuffer'First)'Unchecked_Access,
--                                 cbSendLength  => 7,
--                                 pioRecvPci    => SCARD_PCI_T1'Access,
--                                 pbRecvBuffer  => pbRecvBuffer
--                                   (pbRecvBuffer'First)'Unchecked_Access,
--                                 pcbRecvLength => pcbRecvLength'Access);
--           if ret = SCARD_S_SUCCESS then
--              Ada.Text_IO.Put_Line ("transmit send ok");
--           end if;
--           Ada.Text_IO.Put_Line ("response from card: " &
--             String (To_String
--               (Given => pbRecvBuffer,
--                Len   => 2 * Integer (pcbRecvLength))));
--        end;
--
--        --  End transaction.
--        ret := SCardEndTransaction (hCard         => hCard,
--                                    dwDisposition => SCARD_LEAVE_CARD);
--        if ret = SCARD_S_SUCCESS then
--           Ada.Text_IO.Put_Line ("transaction end ok");
--        end if;
--
--        --  Disconnect.
--        ret := SCardDisconnect (hCard         => hCard,
--                                dwDisposition => SCARD_RESET_CARD);
--
--        if ret = SCARD_S_SUCCESS then
--           Ada.Text_IO.Put_Line ("disconnect ok");
--        end if;
--
--        --  Free memory.
--        C.Strings.Free (Reader_Name);
--     end;
--
--     ret := SCardReleaseContext (hContext => hContext);
--     if ret = SCARD_S_SUCCESS then
--        Ada.Text_IO.Put_Line ("context released");
--     end if;
--  end Runner;
