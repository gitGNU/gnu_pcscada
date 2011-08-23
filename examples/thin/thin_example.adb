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

with Interfaces.C;
with Interfaces.C.Strings;

with PCSC.Thin; use PCSC.Thin;
with PCSC.SCard.Utils;

use PCSC;

--  Thin-binding example
procedure Thin_Example is

   pragma Linker_Options ("-lpcsclite");

   package SCU renames SCard.Utils;

   use Interfaces.C;

   hContext : aliased SCARDCONTEXT;
   ret      : DWORD;

begin

   ret := SCardEstablishContext
     (dwScope   => SCARD_SCOPE_SYSTEM,
      phContext => hContext'Access);
   if ret = SCARD_S_SUCCESS then
      Ada.Text_IO.Put_Line ("context established");
   else
      Ada.Text_IO.Put_Line ("could not establish context");
      return;
   end if;

   ret := SCardIsValidContext (hContext => hContext);
   if ret = SCARD_S_SUCCESS then
      Ada.Text_IO.Put_Line ("context is valid");
   else
      Ada.Text_IO.Put_Line ("context not valid");
      return;
   end if;

   declare
      dwReaders        : aliased DWORD;
      mszReaders       : LPSTR;
      hCard            : aliased SCARDHANDLE;
      dwActiveProtocol : aliased DWORD;

      dwState          : aliased DWORD;
      dwProtocol       : aliased DWORD;
      dwAtrLen         : aliased DWORD := MAX_ATR_SIZE;
      dwReaderLen      : aliased DWORD;

      pbAtr            : Byte_Array (1 .. MAX_ATR_SIZE);

      --  Modify Reader_Name according to your test setup

      Reader_Name      : C.Strings.chars_ptr := C.Strings.New_String
        ("OmniKey CardMan 3121 00 00");
   begin

      --  List readers

      ret := SCardListReaders (hContext    => hContext,
                               mszReaders  =>
                                 Interfaces.C.Strings.Null_Ptr,
                               pcchReaders => dwReaders'Access);

      if not (ret = SCARD_S_SUCCESS) then
         Ada.Text_IO.Put_Line ("could not get list of readers");
         return;
      end if;

      --  NULL termination is counted as well

      if dwReaders = 1 then
         Ada.Text_IO.Put_Line ("no readers found");
         return;
      end if;

      mszReaders := C.Strings.To_Chars_Ptr
        (new C.char_array (C.size_t (1) .. C.size_t (dwReaders)));

      ret := SCardListReaders (hContext    => hContext,
                               mszReaders  => mszReaders,
                               pcchReaders => dwReaders'Access);

      if not (ret = SCARD_S_SUCCESS) then
         Ada.Text_IO.Put_Line ("could not get list of readers");
         return;
      end if;

      Ada.Text_IO.Put_Line ("readers: " &
                            Interfaces.C.Strings.Value (mszReaders));

      --  Try to connect

      ret := SCardConnect
        (hContext             => hContext,
         szReader             => Reader_Name,
         dwShareMode          => SCARD_SHARE_SHARED,
         dwPreferredProtocols => SCARD_PROTOCOL_T0 or SCARD_PROTOCOL_T1,
         phCard               => hCard'Access,
         pdwActiveProtocol    => dwActiveProtocol'Access);

      if ret = SCARD_S_SUCCESS then
         Ada.Text_IO.Put_Line ("connect ok");

         --  Try to re-connect

         ret := SCardReconnect
           (hCard                => hCard,
            dwShareMode          => SCARD_SHARE_SHARED,
            dwPreferredProtocols => SCARD_PROTOCOL_T0 or SCARD_PROTOCOL_T1,
            dwInitialization     => SCARD_LEAVE_CARD,
            pdwActiveProtocol    => dwActiveProtocol'Access);
         if ret = SCARD_S_SUCCESS then
            Ada.Text_IO.Put_Line ("re-connect ok");
         end if;
      else
         Ada.Text_IO.Put_Line ("connect to card failed");
         return;
      end if;

      --  Get status

      SCardStatus
        (returnValue    => ret,
         hCard          => hCard,
         mszReaderNames => C.Strings.Null_Ptr,
         pcchReaderLen  => dwReaderLen'Access,
         pdwState       => dwState'Access,
         pdwProtocol    => dwProtocol'Access,
         pbAtr          => Byte_Array (pbAtr),
         pcbAtrLen      => dwAtrLen'Access);

      if ret = SCARD_S_SUCCESS then
         Ada.Text_IO.Put_Line ("card status ok");
         Ada.Text_IO.Put_Line ("  -- ATR len: " & DWORD'Image (dwAtrLen));
         Ada.Text_IO.Put_Line ("  -- ATR    :  " &
           String (SCU.To_Hex_String (Given => pbAtr,
                                      Len   => 2 * Integer (dwAtrLen))));
         Ada.Text_IO.Put_Line ("  -- STATE  : " & DWORD'Image (dwState));
         Ada.Text_IO.Put_Line ("  -- PROTO  : " &
                               DWORD'Image (dwProtocol));
      else
         Ada.Text_IO.Put_Line ("get status failed");
         return;
      end if;

      --  Begin transaction

      ret := SCardBeginTransaction (hCard => hCard);
      if ret = SCARD_S_SUCCESS then
         Ada.Text_IO.Put_Line ("transaction start ok");
      end if;

      declare
         pbRecvBuffer  : Byte_Array (1 .. 10);
         pbSendBuffer  : constant Byte_Array :=
           (16#00#, 16#A4#, 16#00#, 16#00#, 16#02#, 16#3F#, 16#00#);
         pcbRecvLength : aliased DWORD       := 10;
         pioRecvPCI    : aliased Thin.SCARD_IO_REQUEST;
      begin

         --  Send arbitrary APDU to card

         SCardTransmit
           (returnValue   => ret,
            hCard         => hCard,
            pioSendPci    => SCARD_PCI_T1'Access,
            pbSendBuffer  => pbSendBuffer,
            cbSendLength  => 7,
            pioRecvPci    => pioRecvPCI'Access,
            pbRecvBuffer  => pbRecvBuffer,
            pcbRecvLength => pcbRecvLength'Access);
         if ret = SCARD_S_SUCCESS then
            Ada.Text_IO.Put_Line ("transmit send ok");
         end if;
         Ada.Text_IO.Put_Line ("response from card: " &
           String (SCU.To_Hex_String (Given => pbRecvBuffer,
                                      Len   => 2 * Integer (pcbRecvLength))));
      end;

      --  End transaction

      ret := SCardEndTransaction (hCard         => hCard,
                                  dwDisposition => SCARD_LEAVE_CARD);
      if ret = SCARD_S_SUCCESS then
         Ada.Text_IO.Put_Line ("transaction end ok");
      end if;

      --  Disconnect

      ret := SCardDisconnect (hCard         => hCard,
                              dwDisposition => SCARD_RESET_CARD);

      if ret = SCARD_S_SUCCESS then
         Ada.Text_IO.Put_Line ("disconnect ok");
      end if;

      --  Free memory

      C.Strings.Free (Reader_Name);
   end;

   ret := SCardReleaseContext (hContext => hContext);
   if ret = SCARD_S_SUCCESS then
      Ada.Text_IO.Put_Line ("context released");
   end if;
end Thin_Example;
