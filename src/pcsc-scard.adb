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

with Ada.Exceptions;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;

with GNAT.String_Split;

with PCSC.Thin.Reader;

package body PCSC.SCard is

   use IC;

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

   function To_C (States : in Reader_Status_Set := Empty_Reader_Status_Set)
                  return Thin.READERSTATE_Array;
   --  Convert Ada type Reader_Status_Set to the corresponding C
   --  READERSTATE_ARRAY.

   function To_LPSTR (Reader : in Reader_ID) return IC.Strings.chars_ptr;
   --  Return a new C compatible string from Reader_ID. The allocated memory
   --  must be freed by calling Free.

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


   -----------------------
   -- Establish_Context --
   -----------------------

   procedure Establish_Context
     (Context : in out SCard.Context;
      Scope   : in SCard.Scope)
   is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardEstablishContext
        (dwScope     => C_Scope (Scope),
         phContext   => Context.hContext'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Context failed");
      end if;
      Store_Error (Code => Res);
   end Establish_Context;

   ---------------------
   -- Release_Context --
   ---------------------

   procedure Release_Context (Context : in out SCard.Context) is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardReleaseContext (hContext => Context.hContext);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Could not release context");
      end if;
      Store_Error (Code => Res);
   end Release_Context;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Context : in SCard.Context) return Boolean is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardIsValidContext (hContext => Context.hContext);

      if Res /= Thin.SCARD_S_SUCCESS then
         Store_Error (Code => Res);
         return False;
      end if;
      Store_Error (Code => Res);

      return True;
   end Is_Valid;

   ------------------
   -- List_Readers --
   ------------------

   function List_Readers (Context : in SCard.Context)
                          return Reader_ID_Set
   is
      Res : Thin.DWORD;
      Len : aliased Thin.DWORD := 0;
   begin
      --  Find out how much space we need for storing
      --  readers friendly names first.

      Res := Thin.SCardListReaders (hContext    => Context.hContext,
                                    mszReaders  => Strings.Null_Ptr,
                                    pcchReaders => Len'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Could not get readers");
      end if;

      declare
         C_Readers : aliased char_array := (1 .. size_t (Len) => <>);
      begin

         --  Get readers for this context

         Res := Thin.SCardListReaders
           (hContext    => Context.hContext,
            mszReaders  => Strings.To_Chars_Ptr (C_Readers'Unchecked_Access),
            pcchReaders => Len'Access);

         if Res /= Thin.SCARD_S_SUCCESS then
            SCard_Exception (Code    => Res,
                             Message => "Could not get readers");
         end if;
         Store_Error (Code => Res);

         --  Convert to Ada types

         declare
            Readers : String := To_Ada (Item     => C_Readers,
                                        Trim_Nul => False);
         begin
            return Slice_Readerstring (To_Slice => Readers);
         end;
      end;
   end List_Readers;

   -------------------
   -- Status_Change --
   -------------------

   procedure Status_Change
     (Context    : in SCard.Context;
      Timeout    : in Natural := 0;
      Status_Set : in out Reader_Status_Set)
   is
      use VORSTP;

      Res       : Thin.DWORD;
      C_Timeout : Thin.DWORD;
      C_States  : Thin.READERSTATE_Array := To_C (States => Status_Set);

      procedure Update_Status_Set (Position : in Cursor);
      --  Forward declaration of Update_Status_Set

      procedure Update_Status_Set (Position : in Cursor) is

         procedure Update_Reader_Status (Element : in out Reader_Status);
         --  Forward declaration of Update_Reader_Status

         procedure Update_Reader_Status (Element : in out Reader_Status) is
         begin
            Element.Event_State     := To_Ada
              (C_States (C_States'First).dwEventState);
            Element.Card_ATR.Data   := ATR_Type
              (C_States (size_t (To_Index (Position))).rgbAtr);
            Element.Card_ATR.Length := ATR_Index
              (C_States (size_t (To_Index (Position))).cbAtr);
         end Update_Reader_Status;

      begin
         Status_Set.Data.Update_Element
           (Position => Position, Process => Update_Reader_Status'Access);
      end Update_Status_Set;

   begin

      if Timeout = 0 then
         C_Timeout := Thin.INFINITE;
      end if;

      if Status_Set = Empty_Reader_Status_Set then
         return;
      else
         Res := Thin.SCardGetStatusChange
           (hContext       => Context.hContext,
            dwTimeout      => C_Timeout,
            rgReaderStates => C_States (C_States'First),
            cReaders       => Thin.DWORD (C_States'Last));
      end if;

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Status change detection failed");
      end if;
      Store_Error (Code => Res);

      --  Update Ada type with values returned by C API function
      --  TODO: what happens when a reader vanishes?

      VORSTP.Iterate (Container => Status_Set.Data,
                      Process   => Update_Status_Set'Access);

      --  Free C_States
      Free (C_States);

   end Status_Change;

   ----------------------
   -- Wait_For_Readers --
   ----------------------

   procedure Wait_For_Readers (Context : in SCard.Context) is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardGetStatusChange
        (hContext       => Context.hContext,
         dwTimeout      => Thin.INFINITE);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Waiting for readers failed");
      end if;
      Store_Error (Code => Res);
   end Wait_For_Readers;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Context : in SCard.Context;
      Card    : in out SCard.Card;
      Reader  : in Reader_ID := Null_Reader_ID;
      Mode    : in SCard.Mode := Share_Shared)
   is
      Res      : Thin.DWORD;
      C_Reader : Thin.LPSTR := To_LPSTR (Reader);
   begin
      Res := Thin.SCardConnect
        (hContext             => Context.hContext,
         szReader             => C_Reader,
         dwShareMode          => C_Mode (Mode),
         dwPreferredProtocols => Thin.SCARD_PROTOCOL_T1 or
                                 Thin.SCARD_PROTOCOL_T0,
         phCard               => Card.hCard'Access,
         pdwActiveProtocol    => Card.Active_Proto'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         Strings.Free (C_Reader);
         SCard_Exception (Code    => Res,
                          Message => "Could not connect to reader");
      end if;
      Store_Error (Code => Res);

      --  Free allocated memory

      Strings.Free (C_Reader);
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Card : in SCard.Card; Action : in SCard.Action) is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardDisconnect (hCard         => Card.hCard,
                                   dwDisposition => C_Action (Action));

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Could not disconnect from reader");
      end if;
      Store_Error (Code => Res);
   end Disconnect;

   ---------------
   -- Reconnect --
   ---------------

   procedure Reconnect
     (Card   : in out SCard.Card;
      Mode   : in SCard.Mode;
      Action : in SCard.Action)
   is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardReconnect
        (hCard                => Card.hCard,
         dwShareMode          => C_Mode (Mode),
         dwPreferredProtocols => Thin.SCARD_PROTOCOL_T1 or
                                 Thin.SCARD_PROTOCOL_T0,
         dwInitialization     => C_Action (Action),
         pdwActiveProtocol    => Card.Active_Proto'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Could not reconnect to reader");
      end if;
      Store_Error (Code => Res);
   end Reconnect;

   -----------------------
   -- Begin_Transaction --
   -----------------------

   procedure Begin_Transaction (Card : in SCard.Card) is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardBeginTransaction (hCard => Card.hCard);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Begin of transaction failed");
      end if;
      Store_Error (Code => Res);
   end Begin_Transaction;

   ---------------------
   -- End_Transaction --
   ---------------------

   procedure End_Transaction
     (Card   : in SCard.Card;
      Action : in SCard.Action)
   is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardEndTransaction
        (hCard         => Card.hCard,
         dwDisposition => C_Action (Action));

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "End of transaction failed");
      end if;
      Store_Error (Code => Res);
   end End_Transaction;

   ------------
   -- Status --
   ------------

   procedure Status
     (Card    : in SCard.Card;
      State   : in out SCard.Card_States_Set;
      Proto   : in out SCard.Proto;
      Atr     : in out SCard.ATR)
   is
      Res         : Thin.DWORD;

      dwReaderLen : aliased Thin.DWORD;
      dwState     : aliased Thin.DWORD;
      dwProtocol  : aliased Thin.DWORD;
      dwAtrLen    : aliased Thin.DWORD := Thin.MAX_ATR_SIZE;
   begin
      Res := Thin.SCardStatus
        (hCard          => Card.hCard,
         mszReaderNames => Strings.Null_Ptr,
         pcchReaderLen  => dwReaderLen'Access,
         pdwState       => dwState'Unchecked_Access,
         pdwProtocol    => dwProtocol'Access,
         pbAtr          => Atr.Data (Atr.Data'First)'Unchecked_Access,
         pcbAtrLen      => dwAtrLen'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Get status failed");
      end if;
      Store_Error (Code => Res);

      --  Assign in out params

      Atr.Length := ATR_Index (dwAtrLen);
      Proto      := To_Ada (dwProtocol);
      State      := To_Ada (dwState);
   end Status;

   --------------
   -- Transmit --
   --------------

   procedure Transmit
     (Card        : in SCard.Card;
      Send_Pci    : in PCI;
      Send_Buffer : in out Byte_Set;
      Recv_Pci    : in PCI;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural)
   is
      Res            : Thin.DWORD;

      C_Send_PCI     : aliased Thin.SCARD_IO_REQUEST := C_PCI (Send_Pci);
      C_Recv_PCI     : aliased Thin.SCARD_IO_REQUEST := C_PCI (Recv_Pci);
      Bytes_Returned : aliased Thin.DWORD := Thin.DWORD (Recv_Buffer'Last);
   begin
      --  TODO: Send_Buffer needs to be 'in out', otherwise:
      --        access-to-variable designates constant => fix
      Res := Thin.SCardTransmit
        (hCard         => Card.hCard,
         pioSendPci    => C_Send_PCI'Access,
         pbSendBuffer  => Send_Buffer (Send_Buffer'First)'Unchecked_Access,
         cbSendLength  => Thin.DWORD (Send_Buffer'Length),
         pioRecvPci    => C_Recv_PCI'Access,
         pbRecvBuffer  => Recv_Buffer (Recv_Buffer'First)'Unchecked_Access,
         pcbRecvLength => Bytes_Returned'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Transmit failed");
      end if;
      Store_Error (Code => Res);

      --  Return read bytes count

      Recv_Len := Natural (Bytes_Returned);
   end Transmit;

   -------------
   -- Control --
   -------------

   procedure Control
     (Card        : in SCard.Card;
      Code        : in Natural;
      Send_Buffer : in out Byte_Set;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural)
   is
      Res            : Thin.DWORD;

      Recv_Length    : aliased Thin.DWORD := Thin.DWORD (Recv_Buffer'Last);
      Bytes_Returned : aliased Thin.DWORD := 0;
   begin
      --  TODO: Send_Buffer needs to be 'in out', otherwise:
      --        access-to-variable designates constant => fix
      Res := Thin.SCardControl
        (hCard           => Card.hCard,
         dwControlCode   => Thin.DWORD (Code),
         pbSendBuffer    => Send_Buffer (Send_Buffer'First)'Unchecked_Access,
         cbSendLength    => Thin.DWORD (Send_Buffer'Length),
         pbRecvBuffer    => Recv_Buffer (Recv_Buffer'First)'Unchecked_Access,
         cbRecvLength    => Recv_Length,
         lpBytesReturned => Bytes_Returned'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Sending Control to Card failed");
      end if;
      Store_Error (Code => Res);

      --  Return read bytes count

      Recv_Len := Natural (Bytes_Returned);
   end Control;

   -------------------
   -- Get_Attribute --
   -------------------

   procedure Get_Attribute
     (Card        : in SCard.Card;
      Attr        : in Attribute;
      Recv_Buffer : in out Byte_Set)
   is
      Res : Thin.DWORD;

      Len : aliased Thin.DWORD := Recv_Buffer'Length;
   begin
      Res := Thin.SCardGetAttrib
        (hCard      => Card.hCard,
         dwAttrId   => C_Attr (Attr),
         pbAttr     => Recv_Buffer (Recv_Buffer'First)'Unchecked_Access,
         pcbAttrLen => Len'Access);
      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Get attribute failed");
      end if;
      Store_Error (Code => Res);
   end Get_Attribute;

   ------------------------
   -- Init_Attribute_Set --
   ------------------------

   function Init_Attribute_Set
     (Card        : in SCard.Card;
      Attr        : in Attribute)
      return Byte_Set
   is
      Res      : Thin.DWORD;

      Len      : aliased Thin.DWORD := 0;
   begin
      Res := Thin.SCardGetAttrib
        (hCard      => Card.hCard,
         dwAttrId   => C_Attr (Attr),
         pbAttr     => null,
         pcbAttrLen => Len'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Init attribute set failed");
      end if;
      Store_Error (Code => Res);

      --  TODO: extended return statement
      declare
         B : Byte_Set (1 .. Positive (Len)) := (others => Null_Byte);
      begin
         return B;
      end;
   end Init_Attribute_Set;

   ----------------------
   -- Get_Active_Proto --
   ----------------------

   function Get_Active_Proto (Card : in SCard.Card) return Proto is
   begin
      return To_Ada (Card.Active_Proto);
   end Get_Active_Proto;

   ---------------------------
   -- First (Reader_ID_Set) --
   ---------------------------

   function First (Set : in Reader_ID_Set) return Reader_ID is
      use type Ada.Containers.Count_Type;
   begin
      if Set.Data.Length = 0 then
         return Null_Reader_ID;
      end if;

      return Set.Data.First_Element;
   end First;

   ---------------------------
   -- Empty (Reader_ID_Set) --
   ---------------------------

   function Empty (Set : in Reader_ID_Set) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      if Set.Data.Length = 0 then
         return True;
      end if;

      return False;
   end Empty;

   -------------------------
   -- Add (Reader_Status) --
   -------------------------

   procedure Add
     (States : in out Reader_Status_Set;
      State  : in Reader_Status)
   is
   begin
      States.Data.Append (New_Item => State);
   end Add;

   ------------------------
   -- Add (Reader_State) --
   ------------------------

   procedure Add
     (States : in out Reader_States_Set;
      State  : in Reader_State)
   is
   begin
      States.Data.Append (New_Item => State);
   end Add;

   ----------------------
   -- Add (Card_State) --
   ----------------------

   procedure Add
     (States : in out Card_States_Set;
      State  : in Card_State)
   is
   begin
      States.Data.Append (New_Item => State);
   end Add;

   ------------------------------
   -- Size (Reader_Status_Set) --
   ------------------------------

   function Size (States : in Reader_Status_Set) return Natural is
   begin
      return States.Data.Last_Index;
   end Size;

   ------------
   -- To_Atr --
   ------------

   function To_Atr (Bytes : in Byte_Set) return ATR is
      New_Atr  : ATR;
      Temp_Set : Byte_Set (ATR_Index'Range) := (others => Null_Byte);
   begin
      --  Raise exception if Byte_Set is too big.

      if Bytes'Last > ATR_Index'Last then
         raise Bytes_Too_Big;
      end if;

      --  Store Byte_Set in ATR_Type and set length accordingly.

      Temp_Set (Bytes'First .. Bytes'Last) := Bytes;

      New_Atr.Data   := ATR_Type (Temp_Set);
      New_Atr.Length := Bytes'Length;

      return New_Atr;
   end To_Atr;

   ----------------
   -- Size (ATR) --
   ----------------

   function Size (Atr : in SCard.ATR := Null_ATR) return Natural is
   begin
      return Natural (Atr.Length);
   end Size;

   ----------------
   -- Size (ATR) --
   ----------------

   function Size (Atr : in SCard.ATR := Null_ATR) return String is
      Natural_ATR : Natural := Size (Atr);
   begin
      --  Remove leading space

      return Ada.Strings.Fixed.Trim (Source => Natural'Image (Natural_ATR),
                                     Side   => Ada.Strings.Left);
   end Size;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (States : in Reader_Status_Set;
                        Index  : in Natural)
                        return Reader_Status
   is
   begin
      return States.Data.Element (Index);
   end Get_Status;

   ---------------------
   -- Get_Return_Code --
   ---------------------

   function Get_Return_Code return String is
      Err_Message : constant String := Strings.Value
        (Thin.pcsc_stringify_error (Last_Return_Code));
   begin
      return Err_Message;
   end Get_Return_Code;

   ---------------------
   -- SCard_Exception --
   ---------------------

   procedure SCard_Exception (Code : in Thin.Return_Code; Message : in String)
   is
      Err_Message : constant String := Strings.Value
        (Thin.pcsc_stringify_error (Code));
   begin
      --  Store return code

      Store_Error (Code => Code);

      Ada.Exceptions.Raise_Exception
        (SCard_Error'Identity,
         Message & " - ["
           & Thin.Return_Code'Image (Code) & "] " & Err_Message);
   end SCard_Exception;

   -----------------
   -- Store_Error --
   -----------------

   procedure Store_Error (Code : in Thin.Return_Code) is
   begin
      Last_Return_Code := Code;
   end Store_Error;

   ------------------------
   -- Slice_Readerstring --
   ------------------------

   function Slice_Readerstring (To_Slice : in String) return Reader_ID_Set
   is
      use type GNAT.String_Split.Slice_Number;

      Readers  : Reader_ID_Set;
      Lines    : GNAT.String_Split.Slice_Set;
   begin
      --  Slice readers into parts.
      --  Who uses '\0' as separator anyway?

      GNAT.String_Split.Create
        (S          => Lines,
         From       => To_Slice (To_Slice'First .. To_Slice'Last),
         Separators => Ada.Strings.Maps.To_Set (Ada.Characters.Latin_1.NUL),
         Mode       => GNAT.String_Split.Single);

      --  Minus two because \0\0 is used as termination.

      for J in 1 .. GNAT.String_Split.Slice_Count (Lines) - 2 loop
         Readers.Data.Append (New_Item => To_Unbounded_String
                              (GNAT.String_Split.Slice (Lines, J)));
      end loop;

      return Readers;

   end Slice_Readerstring;

   --------------
   -- To_LPSTR --
   --------------

   function To_LPSTR (Reader : in Reader_ID) return Strings.chars_ptr is
   begin
      return Strings.New_String (To_String (Reader));
   end To_LPSTR;

   --------------------
   -- To_Ada (Proto) --
   --------------------

   function To_Ada (C_Protocol : in Thin.DWORD) return Proto is
   begin
      for P in Proto'Range loop
         if C_Proto (P) = C_Protocol then

            --  Return active Proto

            return P;
         end if;
      end loop;

      --  Return 'Undefined' if no active proto found

      return Proto_Undefined;
   end To_Ada;

   --------------------------------
   -- To_Ada (Card_States_Array) --
   --------------------------------

   function To_Ada (C_Cardstate : in Thin.DWORD) return Card_States_Set is
      States : Card_States_Set;
   begin
      for P in C_Card_State'Range loop
         if (C_Cardstate and C_Card_State (P)) /= 0 then
            States.Data.Append (New_Item => P);
         end if;
      end loop;
      return States;
   end To_Ada;

   ----------------------------------
   -- To_Ada (Reader_States_Set) --
   ----------------------------------

   function To_Ada (C_Readerstate : in Thin.DWORD) return Reader_States_Set is
      States : Reader_States_Set;
   begin
      for P in C_Reader_State'Range loop
         if (C_Readerstate and C_Reader_State (P)) /= 0 then
            States.Data.Append (New_Item => P);
         end if;
      end loop;
      return States;
   end To_Ada;

   --------------------------------
   -- To_C (Reader_Status_Set) --
   --------------------------------

   function To_C (States : in Reader_Status_Set := Empty_Reader_Status_Set)
                  return Thin.READERSTATE_Array
   is
      use VORSTP;

      Position : Cursor := States.Data.First;
      C_States : Thin.READERSTATE_Array
        (size_t (1) .. size_t (States.Data.Last_Index));
   begin
      while Has_Element (Position) loop
         declare
            Item : constant Reader_Status := Element (Position);
         begin
            C_States (size_t (To_Index (Position))) :=
              new Thin.READERSTATE'
                (szReader       => Strings.New_String
                     (To_String (Item.Name)),
                 pvUserData     => <>,  --  use default
                 dwCurrentState => C_Reader_State (Item.Current_State),
                 dwEventState   => <>,  --  use default
                 cbAtr          => Item.Card_ATR'Size,
                 rgbAtr         => Thin.Byte_Array (Item.Card_ATR.Data));

            Next (Position);
         end;
      end loop;

      return C_States;
   end To_C;

   ------------------------------
   -- Free (READERSTATE_Array) --
   ------------------------------

   procedure Free (Name : in out Thin.READERSTATE_Array) is
   begin
      for Index in Name'Range loop
         Strings.Free (Name (Index).szReader);
         Free (Name (Index));
      end loop;
   end Free;
end PCSC.SCard;
