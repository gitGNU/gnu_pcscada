--
--  Copyright (c) 2010,
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

with PCSC.Thin.Reader;

package body IFD is

   Fake_ATR : constant PCSC.Thin.ATR :=
     (16#3B#, 16#FA#, 16#13#, 16#00#, 16#FF#,
      16#81#, 16#31#, 16#80#, 16#45#, 16#00#,
      16#31#, 16#C1#, 16#73#, 16#C0#, 16#01#,
      16#00#, 16#00#, 16#90#, 16#00#, 16#B1#,
      others => 0);
   --  Fake OpenPGP ATR

   -------------------------------------------------------------------------

   function IFDHCloseChannel (Lun : DWORD) return IFDHandler.RESPONSECODE
   is
      pragma Unreferenced (Lun);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHCloseChannel");
      return IFDHandler.IFD_SUCCESS;
   end IFDHCloseChannel;

   -------------------------------------------------------------------------

   function IFDHControl
     (Lun      : DWORD;
      TxBuffer : Value_Access;
      TxLength : DWORD;
      RxBuffer : Value_Access;
      RxLength : PDWORD)
      return IFDHandler.RESPONSECODE
   is
      pragma Unreferenced (Lun, TxBuffer, TxLength, RxBuffer, RxLength);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHControl");
      return IFDHandler.IFD_SUCCESS;
   end IFDHControl;

   -------------------------------------------------------------------------

   function IFDHCreateChannel
     (Lun     : DWORD;
      Channel : DWORD)
      return PCSC.Thin.IFDHandler.RESPONSECODE
   is
      pragma Unreferenced (Lun, Channel);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHCreateChannel");
      return IFDHandler.IFD_SUCCESS;
   end IFDHCreateChannel;

   -------------------------------------------------------------------------

   function IFDHCreateChannelByName
     (Lun        : DWORD;
      DeviceName : LPSTR)
      return PCSC.Thin.IFDHandler.RESPONSECODE
   is
      pragma Unreferenced (Lun, DeviceName);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHCreateChannelByName");
      return IFDHandler.IFD_SUCCESS;
   end IFDHCreateChannelByName;

   -------------------------------------------------------------------------

   procedure IFDHGetCapabilities
     (Result : out DWORD;
      Lun    :     DWORD;
      Tag    :     DWORD;
      Length :     PDWORD;
      Value  :     Value_Access)
   is
      pragma Unreferenced (Lun);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHGetCapabilities");
      if Length = null then
         Result := IFDHandler.IFD_ERROR_TAG;
         return;
      end if;

      Result := IFDHandler.IFD_SUCCESS;

      if Value = null then

         --  Client wants to know the attribute's data length. Fake it.

         Length.all := MAX_ATR_SIZE;
         return;
      end if;

      case Tag is
         when IFDHandler.TAG_IFD_ATR =>
            Ada.Text_IO.Put_Line ("Ada:IFDHGetCapabilities:TAG_IFD_ATR");

         when IFDHandler.TAG_IFD_SLOTS_NUMBER =>
            Ada.Text_IO.Put_Line
              ("Ada:IFDHGetCapabilities:TAG_IFD_SLOTS_NUMBER");

         when IFDHandler.TAG_IFD_SIMULTANEOUS_ACCESS =>
            Ada.Text_IO.Put_Line
              ("Ada:FDHGetCapabilities:TAG_IFD_SIMULTANEOUS_ACCESS");

         when Reader.SCARD_ATTR_VENDOR_NAME =>
            Ada.Text_IO.Put_Line
              ("Ada:FDHGetCapabilities:SCARD_ATTR_VENDOR_NAME");
            Value.all  := Value_Array'(16#41#, 16#64#, 16#61#, others => 0);
            Length.all := 3;

         when Reader.SCARD_ATTR_ATR_STRING =>
            Ada.Text_IO.Put_Line
              ("Ada:FDHGetCapabilities:SCARD_ATTR_ATR_STRING");
            Value.all  := Fake_ATR;
            Length.all := 20;

         when Reader.SCARD_ATTR_MAXINPUT =>
            Ada.Text_IO.Put_Line
              ("Ada:FDHGetCapabilities:SCARD_ATTR_MAXINPUT");
            Value.all  := Value_Array'(16#12#, 16#24#, others => 0);
            Length.all := 2;

         when others =>
            Ada.Text_IO.Put_Line
              ("Ada:FDHGetCapabilities:UNHANDLED_TAG:" & Tag'Img);
            Result := IFDHandler.IFD_ERROR_TAG;
      end case;
   end IFDHGetCapabilities;

   -------------------------------------------------------------------------

   function IFDHICCPresence (Lun : DWORD) return IFDHandler.RESPONSECODE
   is
      pragma Unreferenced (Lun);
   begin
      return IFDHandler.IFD_SUCCESS;
   end IFDHICCPresence;

   -------------------------------------------------------------------------

   procedure IFDHPowerICC
     (Result    : out IFDHandler.RESPONSECODE;
      Lun       :     DWORD;
      Action    :     DWORD;
      Atr       :     ATR_Access;
      AtrLength :     PDWORD)
   is
      pragma Unreferenced (Lun, Action);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHPowerICC");

      --  Return fake OpenPGP ATR

      Atr.all       := Fake_ATR;
      AtrLength.all := 20;

      Result := IFDHandler.IFD_SUCCESS;
   end IFDHPowerICC;

   -------------------------------------------------------------------------

   function IFDHSetCapabilities
     (Lun    : DWORD;
      Tag    : DWORD;
      Length : DWORD;
      Value  : Value_Access)
      return IFDHandler.RESPONSECODE
   is
      pragma Unreferenced (Lun, Tag, Length, Value);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHSetCapabilities");
      return IFDHandler.IFD_SUCCESS;
   end IFDHSetCapabilities;

   -------------------------------------------------------------------------

   function IFDHSetProtocolParameters
     (Lun      : DWORD;
      Protocol : DWORD;
      Flags    : UCHAR;
      PTS1     : UCHAR;
      PTS2     : UCHAR;
      PTS3     : UCHAR)
      return IFDHandler.RESPONSECODE
   is
      pragma Unreferenced (Lun, Protocol, Flags, PTS1, PTS2, PTS3);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHSetProtocolParameters");
      return IFDHandler.IFD_SUCCESS;
   end IFDHSetProtocolParameters;

   -------------------------------------------------------------------------

   function IFDHTransmitToICC
     (Lun      : DWORD;
      SendPci  : IFDHandler.SCARD_IO_HEADER;
      TxBuffer : Value_Access;
      TxLength : DWORD;
      RxBuffer : Value_Access;
      RxLength : PDWORD;
      RecvPci  : IFDHandler.PSCARD_IO_HEADER)
      return IFDHandler.RESPONSECODE
   is
      pragma Unreferenced (Lun, SendPci, TxBuffer, TxLength,
                           RxBuffer, RxLength, RecvPci);
   begin
      Ada.Text_IO.Put_Line ("Ada:IFDHTransmitToICC");
      return IFDHandler.IFD_SUCCESS;
   end IFDHTransmitToICC;

end IFD;
