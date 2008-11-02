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
--  <PURPOSE>
--    This package keeps a list of attributes which can be requested directly
--    from the IFD handler. Use the @Init_Attribute_Set@ and @Get_Attribute@
--    functions to do so. It also provides the constants and types needed for
--    feature handling and PIN verification operations.
--  </PURPOSE>
--

package PCSC.Thin.Reader is

   SCARD_ATTR_VENDOR_NAME            : constant := 16#0001_0100#;
   --  Vendor name

   SCARD_ATTR_VENDOR_IFD_TYPE        : constant := 16#0001_0101#;
   --  Vendor-supplied interface device type (model designation of reader)

   SCARD_ATTR_VENDOR_IFD_VERSION     : constant := 16#0001_0102#;
   --  Vendor-supplied interface device version (DWORD in the form 0xMMmmbbbb
   --  where MM = major version, mm = minor version, and bbbb = build number).

   SCARD_ATTR_VENDOR_IFD_SERIAL_NO   : constant := 16#0001_0103#;
   --  Vendor-supplied interface device serial number

   SCARD_ATTR_CHANNEL_ID             : constant := 16#0002_0110#;
   --  DWORD encoded as 0xDDDDCCCC, where DDDD = data channel type and CCCC =
   --  channel number.

   SCARD_ATTR_DEFAULT_CLK            : constant := 16#0003_0121#;
   --  Default clock rate, in kHz

   SCARD_ATTR_MAX_CLK                : constant := 16#0003_0122#;
   --  Maximum clock rate, in kHz

   SCARD_ATTR_DEFAULT_DATA_RATE      : constant := 16#0003_0123#;
   --  Default data rate, in bps

   SCARD_ATTR_MAX_DATA_RATE          : constant := 16#0003_0124#;
   --  Maximum data rate, in bps

   SCARD_ATTR_MAX_IFSD               : constant := 16#0003_0125#;
   --  Maximum bytes for information file size device

   SCARD_ATTR_POWER_MGMT_SUPPORT     : constant := 16#0004_0131#;
   --  Zero if device does not support power down while smart card is inserted.
   --  Nonzero otherwise.

   SCARD_ATTR_CHARACTERISTICS        : constant := 16#0006_0150#;
   --  DWORD indicating which mechanical characteristics are supported. If
   --  zero, no special characteristics are supported. Note that multiple bits
   --  can be set.

   SCARD_ATTR_CURRENT_PROTOCOL_TYPE  : constant := 16#0008_0201#;
   --  Current protocol type in use

   SCARD_ATTR_CURRENT_CLK            : constant := 16#0008_0202#;
   --  Current clock rate, in kHz

   SCARD_ATTR_CURRENT_F              : constant := 16#0008_0203#;
   --  Clock conversion factor

   SCARD_ATTR_CURRENT_D              : constant := 16#0008_0204#;
   --  Bit rate conversion factor

   SCARD_ATTR_CURRENT_N              : constant := 16#0008_0205#;
   --  Current guard time

   SCARD_ATTR_CURRENT_W              : constant := 16#0008_0206#;
   --  Current work waiting time

   SCARD_ATTR_CURRENT_IFSC           : constant := 16#0008_0207#;
   --  Current byte size for information field size card

   SCARD_ATTR_CURRENT_IFSD           : constant := 16#0008_0208#;
   --  Current byte size for information field size device

   SCARD_ATTR_CURRENT_BWT            : constant := 16#0008_0209#;
   --  Current block waiting time

   SCARD_ATTR_CURRENT_CWT            : constant := 16#0008_020A#;
   --  Current character waiting time

   SCARD_ATTR_CURRENT_EBC_ENCODING   : constant := 16#0008_020B#;
   --  Current error block control encoding

   SCARD_ATTR_EXTENDED_BWT           : constant := 16#0008_020C#;
   --  Extended block waiting time

   SCARD_ATTR_ICC_PRESENCE           : constant := 16#0009_0300#;
   --  Single byte indicating smart card presence

   SCARD_ATTR_ICC_INTERFACE_STATUS   : constant := 16#0009_0301#;
   --  Single byte. Zero if smart card electrical contact is not active;
   --  nonzero if contact is active.

   SCARD_ATTR_CURRENT_IO_STATE       : constant := 16#0009_0302#;

   SCARD_ATTR_ATR_STRING             : constant := 16#0009_0303#;
   --  Answer to reset (ATR) string

   SCARD_ATTR_ICC_TYPE_PER_ATR       : constant := 16#0009_0304#;
   --  Single byte indicating smart card type

   SCARD_ATTR_ESC_RESET              : constant := 16#0007_A000#;

   SCARD_ATTR_ESC_CANCEL             : constant := 16#0007_A003#;

   SCARD_ATTR_ESC_AUTHREQUEST        : constant := 16#0007_A005#;

   SCARD_ATTR_MAXINPUT               : constant := 16#0007_A007#;

   SCARD_ATTR_DEVICE_UNIT            : constant := 16#7FFF_0001#;
   --  Instance of this vendor's reader attached to the computer. The first
   --  instance will be device unit 0, the next will be unit 1 (if it is the
   --  same brand of reader) and so on. Two different brands of readers will
   --  both have zero for this value.

   SCARD_ATTR_DEVICE_IN_USE          : constant := 16#7FFF_0002#;
   --  Reserved for future use

   SCARD_ATTR_DEVICE_FRIENDLY_NAME_A : constant := 16#7FFF_0003#;

   SCARD_ATTR_DEVICE_SYSTEM_NAME_A   : constant := 16#7FFF_0004#;

   SCARD_ATTR_DEVICE_FRIENDLY_NAME_W : constant := 16#7FFF_0005#;

   SCARD_ATTR_DEVICE_SYSTEM_NAME_W   : constant := 16#7FFF_0006#;

   SCARD_ATTR_SUPRESS_T1_IFS_REQUEST : constant := 16#7FFF_0007#;


   CM_IOCTL_GET_FEATURE_REQUEST      : constant DWORD := SCARD_CTL_CODE (3400);
   --  Control code used to get features from card / reader.

   type PCSC_TLV_STRUCTURE is record
      tag    : Interfaces.Unsigned_8;
      length : Interfaces.Unsigned_8;
      value  : Interfaces.Unsigned_32;
      --  This value is always in BIG ENDIAN format as documented in PCSC v2
      --  part 10 ch 2.2 page 2.
   end record;
   --  PCSC Type-length-value data structure

end PCSC.Thin.Reader;
