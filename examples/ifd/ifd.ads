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

with PCSC.Thin.IFDHandler;

package IFD is

   use PCSC.Thin;

   subtype Value_Array is Byte_Array (0 .. MAX_ATR_SIZE);
   --  Transfer data, use max ATR size as upper constraint for now

   type Value_Access is access Value_Array;
   --  Access to transfer data array

   function IFDHCloseChannel (Lun : DWORD) return IFDHandler.RESPONSECODE;
   pragma Export (Convention    => C,
                  Entity        => IFDHCloseChannel,
                  External_Name => "IFDHCloseChannel");

   function IFDHControl
     (Lun      : DWORD;
      TxBuffer : Value_Access;
      TxLength : DWORD;
      RxBuffer : Value_Access;
      RxLength : PDWORD)
      return IFDHandler.RESPONSECODE;
   pragma Export (Convention    => C,
                  Entity        => IFDHControl,
                  External_Name => "IFDHControl");

   function IFDHCreateChannel
     (Lun     : DWORD;
      Channel : DWORD)
      return IFDHandler.RESPONSECODE;
   pragma Export (Convention    => C,
                  Entity        => IFDHCreateChannel,
                  External_Name => "IFDHCreateChannel");

   function IFDHCreateChannelByName
     (Lun        : DWORD;
      DeviceName : LPSTR)
      return IFDHandler.RESPONSECODE;
   pragma Export (Convention    => C,
                  Entity        => IFDHCreateChannelByName,
                  External_Name => "IFDHCreateChannelByName");

   procedure IFDHGetCapabilities
     (Result : out DWORD;
      Lun    :     DWORD;
      Tag    :     DWORD;
      Length :     PDWORD;
      Value  :     Value_Access);
   pragma Export (Convention    => C,
                  Entity        => IFDHGetCapabilities,
                  External_Name => "IFDHGetCapabilities");
   pragma Export_Valued_Procedure (Internal => IFDHGetCapabilities,
                                   External => "IFDHGetCapabilities");

   function IFDHICCPresence (Lun : DWORD) return IFDHandler.RESPONSECODE;
   pragma Export (Convention    => C,
                  Entity        => IFDHICCPresence,
                  External_Name => "IFDHICCPresence");

   procedure IFDHPowerICC
     (Result    : out IFDHandler.RESPONSECODE;
      Lun       :     DWORD;
      Action    :     DWORD;
      Atr       :     ATR_Access;
      AtrLength :     PDWORD);
   pragma Export (Convention    => C,
                  Entity        => IFDHPowerICC,
                  External_Name => "IFDHPowerICC");
   pragma Export_Valued_Procedure (Internal => IFDHPowerICC,
                                   External => "IFDHPowerICC");

   function IFDHSetCapabilities
     (Lun    : DWORD;
      Tag    : DWORD;
      Length : DWORD;
      Value  : Value_Access)
      return IFDHandler.RESPONSECODE;
   pragma Export (Convention    => C,
                  Entity        => IFDHSetCapabilities,
                  External_Name => "IFDHSetCapabilities");

   function IFDHSetProtocolParameters
     (Lun      : DWORD;
      Protocol : DWORD;
      Flags    : UCHAR;
      PTS1     : UCHAR;
      PTS2     : UCHAR;
      PTS3     : UCHAR)
      return IFDHandler.RESPONSECODE;
   pragma Export (Convention    => C,
                  Entity        => IFDHSetProtocolParameters,
                  External_Name => "IFDHSetProtocolParameters");

   function IFDHTransmitToICC
     (Lun      : DWORD;
      SendPci  : IFDHandler.SCARD_IO_HEADER;
      TxBuffer : Value_Access;
      TxLength : DWORD;
      RxBuffer : Value_Access;
      RxLength : PDWORD;
      RecvPci  : IFDHandler.PSCARD_IO_HEADER)
      return IFDHandler.RESPONSECODE;
   pragma Export (Convention    => C,
                  Entity        => IFDHTransmitToICC,
                  External_Name => "IFDHTransmitToICC");

end IFD;
