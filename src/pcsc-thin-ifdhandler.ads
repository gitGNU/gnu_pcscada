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
--  <PURPOSE>
--    Thin binding to the IFD handler API.
--  </PURPOSE>
--

package PCSC.Thin.IFDHandler is

   subtype RESPONSECODE is C.long;
   --  IFD response codes

   IFD_SUCCESS                     : constant := 0;
   --  No error

   IFD_ERROR_TAG                   : constant := 600;
   --  Tag unknown

   TAG_IFD_ATR                     : constant := 16#0303#;
   --  ATR

   TAG_IFD_SLOTNUM                 : constant := 16#0180#;
   --  Select a slot

   TAG_IFD_SLOT_THREAD_SAFE        : constant := 16#0FAC#;
   --  Support access to different slots of the reader

   TAG_IFD_THREAD_SAFE             : constant := 16#0FAD#;
   --  Driver is thread safe

   TAG_IFD_SLOTS_NUMBER            : constant := 16#0FAE#;
   --  Number of slots of the reader

   TAG_IFD_SIMULTANEOUS_ACCESS     : constant := 16#0FAF#;
   --  Number of readers the driver can manage

   TAG_IFD_POLLING_THREAD          : constant := 16#0FB0#;
   --  Driver uses a polling thread

   TAG_IFD_POLLING_THREAD_KILLABLE : constant := 16#0FB1#;
   --  The polling thread can be killed

   type SCARD_IO_HEADER is record
      Protocol : DWORD;
      Length   : DWORD;
   end record;

   type PSCARD_IO_HEADER is access SCARD_IO_HEADER;

end PCSC.Thin.IFDHandler;
