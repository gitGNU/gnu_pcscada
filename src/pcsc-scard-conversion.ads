--
--  Copyright (c) 2008,
--  Reto Buerki <buerki@swiss-it.ch>
--
--  This file is part of PCSC/Ada.
--
--  PCSC/Ada is free software; you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Publ License as published
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

with Ada.Unchecked_Deallocation;

with PCSC.Thin.Reader;

--  This package contains all mapping information and related conversion helper
--  functions to convert types from Ada to C and vice versa. The package is
--  declared private and cannot be included from the client code.
private package PCSC.SCard.Conversion is

   C_Scope : constant array (Scope) of Thin.DWORD
     := (Scope_User     => Thin.SCARD_SCOPE_USER,
         Scope_Terminal => Thin.SCARD_SCOPE_TERMINAL,
         Scope_System   => Thin.SCARD_SCOPE_SYSTEM);
   --  Map Scope to corresponding C values

   C_Mode : constant array (Mode) of Thin.DWORD
     := (Share_Exclusive => Thin.SCARD_SHARE_EXCLUSIVE,
         Share_Shared    => Thin.SCARD_SHARE_SHARED,
         Share_Direct    => Thin.SCARD_SHARE_DIRECT);
   --  Map Mode to corresponding C values

   C_Proto : constant array (Proto) of Thin.DWORD
     := (Proto_Undefined => Thin.SCARD_PROTOCOL_UNDEFINED,
         Proto_Unset     => Thin.SCARD_PROTOCOL_UNSET,
         Proto_T0        => Thin.SCARD_PROTOCOL_T0,
         Proto_T1        => Thin.SCARD_PROTOCOL_T1,
         Proto_RAW       => Thin.SCARD_PROTOCOL_RAW,
         Proto_T15       => Thin.SCARD_PROTOCOL_T15);
   --  Map Proto to corresponding C values

   C_Action : constant array (Action) of Thin.DWORD
     := (Leave_Card   => Thin.SCARD_LEAVE_CARD,
         Reset_Card   => Thin.SCARD_RESET_CARD,
         Unpower_Card => Thin.SCARD_UNPOWER_CARD,
         Eject_Card   => Thin.SCARD_EJECT_CARD);
   --  Map Action to corresponding C values

   C_Card_State : constant array (Card_State) of Thin.DWORD
     := (S_Card_Unknown    => Thin.SCARD_UNKNOWN,
         S_Card_Absent     => Thin.SCARD_ABSENT,
         S_Card_Present    => Thin.SCARD_PRESENT,
         S_Card_Swallowed  => Thin.SCARD_SWALLOWED,
         S_Card_Powered    => Thin.SCARD_POWERED,
         S_Card_Negotiable => Thin.SCARD_NEGOTIABLE,
         S_Card_Specific   => Thin.SCARD_SPECIFIC);
   --  Map Card_State to corresponding C values.

   C_Reader_State : constant array (Reader_State) of Thin.DWORD
     := (S_Reader_Unaware     => Thin.SCARD_STATE_UNAWARE,
         S_Reader_Ignore      => Thin.SCARD_STATE_IGNORE,
         S_Reader_Changed     => Thin.SCARD_STATE_CHANGED,
         S_Reader_Unknown     => Thin.SCARD_STATE_UNKNOWN,
         S_Reader_Unavailable => Thin.SCARD_STATE_UNAVAILABLE,
         S_Reader_Empty       => Thin.SCARD_STATE_EMPTY,
         S_Reader_Present     => Thin.SCARD_STATE_PRESENT,
         S_Reader_Atrmatch    => Thin.SCARD_STATE_ATRMATCH,
         S_Reader_Exclusive   => Thin.SCARD_STATE_EXCLUSIVE,
         S_Reader_Inuse       => Thin.SCARD_STATE_INUSE,
         S_Reader_Mute        => Thin.SCARD_STATE_MUTE,
         S_Reader_Unpowered   => Thin.SCARD_STATE_UNPOWERED);
   --  Map Reader_State to corresponding C values

   C_PCI : constant array (PCI) of Thin.SCARD_IO_REQUEST
     := (PCI_T0  => Thin.SCARD_PCI_T0,
         PCI_T1  => Thin.SCARD_PCI_T1,
         PCI_RAW => Thin.SCARD_PCI_RAW);
   --  Map PCI to corresponding C SCARD_IO_REQUESTs

   package TR renames Thin.Reader;

   C_Attr : constant array (Attribute) of Thin.DWORD
     := (Attr_Vendor_Name            => TR.SCARD_ATTR_VENDOR_NAME,
         Attr_Vendor_IFD_Type        => TR.SCARD_ATTR_VENDOR_IFD_TYPE,
         Attr_Vendor_IFD_Version     => TR.SCARD_ATTR_VENDOR_IFD_VERSION,
         Attr_Vendor_IFD_Serial      => TR.SCARD_ATTR_VENDOR_IFD_SERIAL_NO,
         Attr_Channel_ID             => TR.SCARD_ATTR_CHANNEL_ID,
         Attr_Default_CLK            => TR.SCARD_ATTR_DEFAULT_CLK,
         Attr_Max_CLK                => TR.SCARD_ATTR_MAX_CLK,
         Attr_Default_Data_Rate      => TR.SCARD_ATTR_DEFAULT_DATA_RATE,
         Attr_Max_Data_Rate          => TR.SCARD_ATTR_MAX_DATA_RATE,
         Attr_Max_IFSD               => TR.SCARD_ATTR_MAX_IFSD,
         Attr_Power_Mgmt_Support     => TR.SCARD_ATTR_POWER_MGMT_SUPPORT,
         Attr_Characteristics        => TR.SCARD_ATTR_CHARACTERISTICS,
         Attr_Current_Protocol_Type  => TR.SCARD_ATTR_CURRENT_PROTOCOL_TYPE,
         Attr_Current_CLK            => TR.SCARD_ATTR_CURRENT_CLK,
         Attr_Current_F              => TR.SCARD_ATTR_CURRENT_F,
         Attr_Current_D              => TR.SCARD_ATTR_CURRENT_D,
         Attr_Current_N              => TR.SCARD_ATTR_CURRENT_N,
         Attr_Current_W              => TR.SCARD_ATTR_CURRENT_W,
         Attr_Current_IFSC           => TR.SCARD_ATTR_CURRENT_IFSC,
         Attr_Current_IFSD           => TR.SCARD_ATTR_CURRENT_IFSD,
         Attr_Current_BWT            => TR.SCARD_ATTR_CURRENT_BWT,
         Attr_Current_CWT            => TR.SCARD_ATTR_CURRENT_CWT,
         Attr_Current_EBC_Encoding   => TR.SCARD_ATTR_CURRENT_EBC_ENCODING,
         Attr_Extended_BWT           => TR.SCARD_ATTR_EXTENDED_BWT,
         Attr_ICC_Presence           => TR.SCARD_ATTR_ICC_PRESENCE,
         Attr_ICC_Interface_Status   => TR.SCARD_ATTR_ICC_INTERFACE_STATUS,
         Attr_Current_IO_State       => TR.SCARD_ATTR_CURRENT_IO_STATE,
         Attr_ATR_String             => TR.SCARD_ATTR_ATR_STRING,
         Attr_ICC_Type_Per_ATR       => TR.SCARD_ATTR_ICC_TYPE_PER_ATR,
         Attr_ESC_Preset             => TR.SCARD_ATTR_ESC_RESET,
         Attr_ESC_Cancel             => TR.SCARD_ATTR_ESC_CANCEL,
         Attr_ESC_Authrequest        => TR.SCARD_ATTR_ESC_AUTHREQUEST,
         Attr_Maxinput               => TR.SCARD_ATTR_MAXINPUT,
         Attr_Device_Unit            => TR.SCARD_ATTR_DEVICE_UNIT,
         Attr_Device_In_Use          => TR.SCARD_ATTR_DEVICE_IN_USE,
         Attr_Device_Friendly_Name_A => TR.SCARD_ATTR_DEVICE_FRIENDLY_NAME_A,
         Attr_Device_System_Name_A   => TR.SCARD_ATTR_DEVICE_SYSTEM_NAME_A,
         Attr_Device_Friendly_Name_W => TR.SCARD_ATTR_DEVICE_FRIENDLY_NAME_W,
         Attr_Device_System_Name_W   => TR.SCARD_ATTR_DEVICE_SYSTEM_NAME_W,
         Attr_Supress_T1_IFS_Request => TR.SCARD_ATTR_SUPRESS_T1_IFS_REQUEST);
   --  Map Attribute to corresponding C values


   function Slice_Readerstring (To_Slice : in String) return Reader_ID_Set;
   --  Slice reader string returned from thin binding and create vector of
   --  reader names. The string to slice has a format like:
   --  Reader A1\0Reader B1\0Reader C1\0\0
   --  \0 is used as separator, \0\0 as string termination.
   --
   --  If an invalid string is passed to Slice_Readerstring, an empty
   --  Reader_ID_Set is returned. In this context, invalid means that To_Slice
   --  is not in the format described above (e.g. no double NUL termination).

   function To_C (States : in Reader_Status_Set) return Thin.READERSTATE_Array;
   --  Convert Ada type Reader_Status_Set to the corresponding C
   --  READERSTATE_Array. Memory allocated by the array must be freed by
   --  calling Free (Thin.READERSTATE_Array) after usage.

   function To_Chars_Ptr (Reader : in Reader_ID) return IC.Strings.chars_ptr;
   --  Return a new C compatible string from Reader_ID. The allocated memory
   --  must be freed by calling Interfaces.C.Strings.Free.

   function To_Ada (C_Protocol : in Thin.DWORD) return Proto;
   --  Return Ada style Proto for C_Protocol (DWORD).

   function To_Ada (C_Cardstate : in Thin.DWORD) return Card_States_Set;
   --  Return Ada style Card_States_Array for C_Cardstate (DWORD).

   function To_Ada (C_Readerstate : in Thin.DWORD) return Reader_States_Set;
   --  Return Ada style Reader_States_Set for C_Readerstate (DWORD).

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Thin.READERSTATE,
      Name   => Thin.READERSTATE_Access);
   --  Free memory allocated by a C READERSTATE struct.

   procedure Free (Name : in out Thin.READERSTATE_Array);
   --  Free C Array of READERSTATES.

end PCSC.SCard.Conversion;
