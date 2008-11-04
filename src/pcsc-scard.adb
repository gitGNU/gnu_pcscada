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
with Ada.Strings.Fixed;

with Interfaces.C;
with Interfaces.C.Strings;

with PCSC.SCard.Conversion;

with PCSC.Thin.Reader;
with PCSC.SCard.Utils;

package body PCSC.SCard is

   use Interfaces.C;

   package Convert renames PCSC.SCard.Conversion;

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
        (dwScope     => Convert.C_Scope (Scope),
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
            Readers : constant String := To_Ada (Item     => C_Readers,
                                                 Trim_Nul => False);
         begin
            return Convert.Slice_Readerstring (To_Slice => Readers);
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
      use type VORSTP.Vector;

      Res       : Thin.DWORD;
      C_Timeout : Thin.DWORD;
      C_States  : Thin.READERSTATE_Array := Convert.To_C
        (States => Status_Set);

      procedure Update_Status_Set (Position : in VORSTP.Cursor);
      --  Forward declaration of Update_Status_Set

      procedure Update_Status_Set (Position : in VORSTP.Cursor) is

         procedure Update_Reader_Status (Element : in out Reader_Status);
         --  Forward declaration of Update_Reader_Status

         procedure Update_Reader_Status (Element : in out Reader_Status) is
         begin
            Element.Event_State     := Convert.To_Ada
              (C_States (C_States'First).dwEventState);
            Element.Card_ATR.Data   := ATR_Type
              (C_States (size_t (VORSTP.To_Index (Position))).rgbAtr);
            Element.Card_ATR.Length := ATR_Index
              (C_States (size_t (VORSTP.To_Index (Position))).cbAtr);
         end Update_Reader_Status;

      begin
         Status_Set.Data.Update_Element
           (Position => Position, Process => Update_Reader_Status'Access);
      end Update_Status_Set;

   begin

      if Timeout = 0 then
         C_Timeout := Thin.INFINITE;
      end if;

      if Status_Set.Data = VORSTP.Empty_Vector then
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
      Convert.Free (Name => C_States);

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
      C_Reader : Thin.LPSTR := Convert.To_Chars_Ptr (Reader => Reader);
   begin
      Res := Thin.SCardConnect
        (hContext             => Context.hContext,
         szReader             => C_Reader,
         dwShareMode          => Convert.C_Mode (Mode),
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
                                   dwDisposition => Convert.C_Action (Action));

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
         dwShareMode          => Convert.C_Mode (Mode),
         dwPreferredProtocols => Thin.SCARD_PROTOCOL_T1 or
                                 Thin.SCARD_PROTOCOL_T0,
         dwInitialization     => Convert.C_Action (Action),
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
         dwDisposition => Convert.C_Action (Action));

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
         pdwState       => dwState'Access,
         pdwProtocol    => dwProtocol'Access,
         pbAtr          => Atr.Data (Atr.Data'First)'Access,
         pcbAtrLen      => dwAtrLen'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Get status failed");
      end if;
      Store_Error (Code => Res);

      --  Assign in out params

      Atr.Length := ATR_Index (dwAtrLen);
      Proto      := Convert.To_Ada (dwProtocol);
      State      := Convert.To_Ada (dwState);
   end Status;

   --------------
   -- Transmit --
   --------------

   procedure Transmit
     (Card        : in SCard.Card;
      Send_Pci    : in PCI;
      Send_Buffer : in Byte_Set := Null_Byte_Set;
      Recv_Pci    : in PCI;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural)
   is
      Res            : Thin.DWORD;

      Send_First     : aliased Thin.Byte;
      Send_First_Ptr : Thin.Byte_Access;

      C_Send_PCI     : aliased Thin.SCARD_IO_REQUEST :=
        Convert.C_PCI (Send_Pci);
      C_Recv_PCI     : aliased Thin.SCARD_IO_REQUEST :=
        Convert.C_PCI (Recv_Pci);
      Bytes_Returned : aliased Thin.DWORD := Thin.DWORD (Recv_Buffer'Last);
   begin
      --  Empty send buffer makes no sense, return without doing anything
      if Send_Buffer = Null_Byte_Set then
         return;
      end if;

      --  Assign first byte of send buffer to local variable 'Send_First'. This
      --  is needed because we cannot directly pass the 'in' Parameter
      --  Send_Buffer as Send_Buffer (Send_Buffer'First)'Access to the thin
      --  binding, otherwise the compiler complains:
      --        access-to-variable designates constant

      Send_First     := Send_Buffer (Send_Buffer'First);
      Send_First_Ptr := Send_First'Unchecked_Access;

      Res := Thin.SCardTransmit
        (hCard         => Card.hCard,
         pioSendPci    => C_Send_PCI'Access,
         pbSendBuffer  => Send_First_Ptr,
         cbSendLength  => Thin.DWORD (Send_Buffer'Length),
         pioRecvPci    => C_Recv_PCI'Access,
         pbRecvBuffer  => Recv_Buffer (Recv_Buffer'First)'Access,
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
      Send_Buffer : in Byte_Set := Null_Byte_Set;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural)
   is
      Res            : Thin.DWORD;

      Send_First     : aliased Thin.Byte;
      Send_First_Ptr : Thin.Byte_Access;

      Recv_Length    : aliased constant Thin.DWORD :=
        Thin.DWORD (Recv_Buffer'Last);
      Bytes_Returned : aliased Thin.DWORD := 0;
   begin
      --  Assign first byte of send buffer to local variable 'Send_First'. This
      --  is needed because we cannot directly pass the 'in' Parameter
      --  Send_Buffer as Send_Buffer (Send_Buffer'First)'Access to the thin
      --  binding, otherwise the compiler complains:
      --        access-to-variable designates constant

      --  The send buffer can also by empty, replace with null ptr if it is
      if Send_Buffer = Null_Byte_Set then
         Send_First_Ptr := null;
      else
         Send_First     := Send_Buffer (Send_Buffer'First);
         Send_First_Ptr := Send_First'Unchecked_Access;
      end if;

      Res := Thin.SCardControl
        (hCard           => Card.hCard,
         dwControlCode   => Thin.DWORD (Code),
         pbSendBuffer    => Send_First_Ptr,
         cbSendLength    => Thin.DWORD (Send_Buffer'Length),
         pbRecvBuffer    => Recv_Buffer (Recv_Buffer'First)'Access,
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
         dwAttrId   => Convert.C_Attr (Attr),
         pbAttr     => Recv_Buffer (Recv_Buffer'First)'Access,
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
      Res : Thin.DWORD;

      Len : aliased Thin.DWORD := 0;
   begin
      Res := Thin.SCardGetAttrib
        (hCard      => Card.hCard,
         dwAttrId   => Convert.C_Attr (Attr),
         pbAttr     => null,
         pcbAttrLen => Len'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Init attribute set failed");
      end if;
      Store_Error (Code => Res);

      --  TODO: extended return statement
      declare
         B : constant Byte_Set (1 .. Positive (Len)) :=
           (others => Thin.Null_Byte);
      begin
         return B;
      end;
   end Init_Attribute_Set;

   ----------------------
   -- Get_Active_Proto --
   ----------------------

   function Get_Active_Proto (Card : in SCard.Card) return Proto is
   begin
      return Convert.To_Ada (Card.Active_Proto);
   end Get_Active_Proto;

   --------------
   -- SPE_Init --
   --------------

   procedure SPE_Init (Card : in out SCard.Card; Result : in out Boolean) is
      package TR renames Thin.Reader;

      Res         : Thin.DWORD;

      Recv_Buffer : Thin.Byte_Array (1 .. Thin.MAX_BUFFER_SIZE);
      Recv_Len    : aliased Thin.DWORD := 0;
      Elements    : Natural := 0;
   begin
      Result := False;

      Res := Thin.SCardControl
        (hCard           => Card.hCard,
         dwControlCode   => TR.CM_IOCTL_GET_FEATURE_REQUEST,
         pbSendBuffer    => null,
         cbSendLength    => 0,
         pbRecvBuffer    => Recv_Buffer (Recv_Buffer'First)'Access,
         cbRecvLength    => Thin.DWORD (Recv_Buffer'Last),
         lpBytesReturned => Recv_Len'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         Store_Error (Code => Res);
         return;
      end if;
      Store_Error (Code => Res);

      --  Verify the result

      if Recv_Len mod (TR.PCSC_TLV_STRUCTURE'Size / 8) > 0 then
         --  Received buffer can not be used, return False
         return;
      end if;

      --  Get number of TLV elements instead of the complete size
      Elements := Integer (Recv_Len) / (TR.PCSC_TLV_STRUCTURE'Size / 8);

      --  Create TLV structure from bytes received
      --  TODO: create a function/procedure to do this job in a more
      --        effective way.

      declare
         use type Interfaces.Unsigned_8;

         TLV_Array : array (1 .. size_t (Elements)) of TR.PCSC_TLV_STRUCTURE;

         T         : size_t := Recv_Buffer'First;
         Index     : size_t := TLV_Array'First;

         Value     : Byte_Set (1 .. 4);
      begin
         loop
            exit when T >= size_t (Recv_Len);
            TLV_Array (Index).tag    := Recv_Buffer (T);
            TLV_Array (Index).length := Recv_Buffer (T + 1);

            --  Value is stored in Big endian format
            Value (4) := Recv_Buffer (T + 2);
            Value (3) := Recv_Buffer (T + 3);
            Value (2) := Recv_Buffer (T + 4);
            Value (1) := Recv_Buffer (T + 5);
            TLV_Array (Index).value  := Interfaces.Unsigned_32
              (Utils.To_Long_Long_Integer (Given => Value));
            T     := T + 6;
            Index := Index + 1;
         end loop;

         --  Test for FEATURE_VERIFY_PIN_DIRECT flag

         for Index in size_t range TLV_Array'Range loop
            if TLV_Array (Index).tag = TR.FEATURE_VERIFY_PIN_DIRECT then
               --  Store verify control code for this card
               Card.Verify_Ctrl := Thin.DWORD (TLV_Array (Index).value);
               Result := True;
               return;
            end if;
         end loop;
      end;
   end SPE_Init;

   --------------
   -- SPE_Exec --
   --------------

   procedure SPE_Exec (Card : in out SCard.Card; Result : in out Byte_Set) is
      package TR renames Thin.Reader;

      Res           : Thin.DWORD;

      Verify_Struct : TR.PIN_VERIFY_STRUCTURE;

      Recv_Buffer   : Thin.Byte_Array (1 .. Thin.MAX_BUFFER_SIZE);
      Recv_Len      : aliased Thin.DWORD := 0;
      Supported     : Boolean := False;
   begin
      --  Check control code of this card
      if Card.Verify_Ctrl = 0 then
         SPE_Init (Card   => Card,
                   Result => Supported);
         if not Supported then
            raise SCard_Not_Supported;
         end if;
      end if;

      --  Construct PC/SC v2.0.2 Part 10 PIN verification data structure
      Verify_Struct.bTimerOut                 := 0;
      Verify_Struct.bTimerOut2                := 0;
      Verify_Struct.bmFormatString            := 16#82#;
      Verify_Struct.bmPINBlockString          := 16#04#;
      Verify_Struct.bmPINLengthFormat         := 0;
      Verify_Struct.wPINMaxExtraDigit         := 16#0408#;
      Verify_Struct.bEntryValidationCondition := 16#02#;
      Verify_Struct.bNumberMessage            := 16#01#;
      Verify_Struct.wLangId                   := 16#0904#;
      Verify_Struct.bMsgIndex                 := 0;
      Verify_Struct.abData (1)                := 16#00#;
      Verify_Struct.abData (2)                := 16#20#;
      Verify_Struct.abData (3)                := 16#00#;
      Verify_Struct.abData (4)                := 16#00#;
      Verify_Struct.abData (5)                := 16#08#;
      Verify_Struct.abData (6)                := 16#30#;
      Verify_Struct.abData (7)                := 16#30#;
      Verify_Struct.abData (8)                := 16#30#;
      Verify_Struct.abData (9)                := 16#30#;
      Verify_Struct.abData (10)               := 16#00#;
      Verify_Struct.abData (11)               := 16#00#;
      Verify_Struct.abData (12)               := 16#00#;
      Verify_Struct.abData (13)               := 16#00#;
      Verify_Struct.ulDataLength              := 13;

      Res := Thin.SCardControl
        (hCard           => Card.hCard,
         dwControlCode   => Card.Verify_Ctrl,
         pbSendBuffer    => Verify_Struct.bTimerOut'Access,
         cbSendLength    => 32, --  Exact size of Verify_Struct object
         pbRecvBuffer    => Recv_Buffer (Recv_Buffer'First)'Access,
         cbRecvLength    => Thin.DWORD (Recv_Buffer'Last),
         lpBytesReturned => Recv_Len'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Verify control failed");
      end if;

      --  Store Bytes returned from reader in Result byte set
      --  TODO: this assumes return code to be two bytes. Fix by using a more
      --        flexible way of Byte_Set size handling (handles to byte sets?)
      for Index in Result'Range loop
         Result (Index) := Recv_Buffer (size_t (Index));
      end loop;
   end SPE_Exec;

   ------------------
   -- To_Reader_ID --
   ------------------

   function To_Reader_ID (Name : in String) return Reader_ID is
   begin
      return Reader_ID'(To_Unbounded_String (Name));
   end To_Reader_ID;

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

   --------------------------
   -- Last (Reader_ID_Set) --
   --------------------------

   function Last (Set : in Reader_ID_Set) return Reader_ID is
      use type Ada.Containers.Count_Type;
   begin
      if Set.Data.Length = 0 then
         return Null_Reader_ID;
      end if;

      return Set.Data.Last_Element;
   end Last;

   -------------------------
   -- Get (Reader_ID_Set) --
   -------------------------

   function Get (Set   : in Reader_ID_Set;
                 Index : in Natural)
                 return Reader_ID
   is
   begin
      --  TODO: bound checks on 'Index'
      return Set.Data.Element (Index);
   end Get;

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
     (Set    : in out Reader_Status_Set;
      Status : in Reader_Status)
   is
   begin
      Set.Data.Append (New_Item => Status);
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

   function Size (Set : in Reader_Status_Set) return Natural is
   begin
      return Set.Data.Last_Index;
   end Size;

   ------------
   -- To_Atr --
   ------------

   function To_Atr (Bytes : in Byte_Set) return ATR is
      New_Atr  : ATR;
      Temp_Set : Byte_Set (ATR_Index'Range) := (others => Thin.Null_Byte);
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
      Natural_ATR : constant Natural := Size (Atr);
   begin
      --  Remove leading space

      return Ada.Strings.Fixed.Trim (Source => Natural'Image (Natural_ATR),
                                     Side   => Ada.Strings.Left);
   end Size;

   -----------------------------
   -- Get (Reader_Status_Set) --
   -----------------------------

   function Get (Set    : in Reader_Status_Set;
                 Index  : in Natural)
                 return Reader_Status
   is
   begin
      --  TODO: bound checks on 'Index'
      return Set.Data.Element (Index);
   end Get;

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

end PCSC.SCard;
