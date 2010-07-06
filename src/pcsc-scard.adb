--
--  Copyright (c) 2008-2009,
--  Reto Buerki <reet@codelabs.ch>
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

   -------------------------------------------------------------------------

   procedure Add
     (States : in out Card_States_Set;
      State  :        Card_State)
   is
   begin
      States.Data.Append (New_Item => State);
   end Add;

   -------------------------------------------------------------------------

   procedure Add
     (States : in out Reader_States_Set;
      State  :        Reader_State)
   is
   begin
      States.Data.Append (New_Item => State);
   end Add;

   -------------------------------------------------------------------------

   procedure Add
     (Set    : in out Reader_Condition_Set;
      Status :        Reader_Condition)
   is
   begin
      Set.Data.Append (New_Item => Status);
   end Add;

   -------------------------------------------------------------------------

   procedure Begin_Transaction (Card : SCard.Card) is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardBeginTransaction (hCard => Card.hCard);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Begin of transaction failed");
      end if;
      Store_Error (Code => Res);
   end Begin_Transaction;

   -------------------------------------------------------------------------

   procedure Cancel (Context : SCard.Context) is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardCancel (hContext => Context.hContext);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Cancel operation failed");
      end if;
      Store_Error (Code => Res);
   end Cancel;

   -------------------------------------------------------------------------

   procedure Connect
     (Context :        SCard.Context;
      Card    : in out SCard.Card;
      Reader  :        Reader_ID  := Null_Reader_ID;
      Mode    :        SCard.Mode := Share_Shared)
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

   -------------------------------------------------------------------------

   procedure Control
     (Card        :        SCard.Card;
      Code        :        Natural;
      Send_Buffer :        Byte_Set := Null_Byte_Set;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural)
   is
      Res            : Thin.DWORD;

      Sbuffer_Copy   : aliased Byte_Set := Send_Buffer;
      --  Copy of initial Send_Buffer. This is needed because we cannot
      --  directly pass the 'in' Parameter Send_Buffer as :
      --    pbSendBuffer => Send_Buffer (Send_Buffer'First)'Access
      --  to the thin binding. If we try, the compiler complains:
      --    access-to-variable designates constant

      Send_First_Ptr : Thin.Byte_Access;
      --  Pointer to first byte of send buffer

      Recv_Length    : aliased constant Thin.DWORD :=
        Thin.DWORD (Recv_Buffer'Last);
      Bytes_Returned : aliased Thin.DWORD          := 0;
   begin

      --  The send buffer can also by empty, replace with null ptr if it is

      if Send_Buffer = Null_Byte_Set then
         Send_First_Ptr := null;
      else
         Send_First_Ptr := Sbuffer_Copy (Sbuffer_Copy'First)'Unchecked_Access;
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

   -------------------------------------------------------------------------

   procedure Disconnect
     (Card   : SCard.Card;
      Action : SCard.Action)
   is
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

   -------------------------------------------------------------------------

   function Empty (Set : Reader_ID_Set) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      if Set.Data.Length = 0 then
         return True;
      end if;

      return False;
   end Empty;

   -------------------------------------------------------------------------

   procedure End_Transaction
     (Card   : SCard.Card;
      Action : SCard.Action)
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

   -------------------------------------------------------------------------

   procedure Establish_Context
     (Context : in out SCard.Context;
      Scope   :        SCard.Scope)
   is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardEstablishContext
        (dwScope   => Convert.C_Scope (Scope),
         phContext => Context.hContext'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Context failed");
      end if;
      Store_Error (Code => Res);
   end Establish_Context;

   -------------------------------------------------------------------------

   function Find
     (Set       : Reader_Condition_Set;
      Reader_ID : SCard.Reader_ID)
      return Boolean
   is
      Position : VORCP.Cursor := Set.Data.First;
      Item     : Reader_Condition;
   begin
      while VORCP.Has_Element (Position) loop
         Item := VORCP.Element (Position);
         if Item.Name = Reader_ID then
            return True;
         end if;
         VORCP.Next (Position);
      end loop;
      return False;
   end Find;

   -------------------------------------------------------------------------

   function First_Index (Set : Reader_ID_Set) return Natural is
   begin
      return Set.Data.First_Index;
   end First_Index;

   -------------------------------------------------------------------------

   function First_Index (Set : Reader_States_Set) return Natural is
   begin
      return Set.Data.First_Index;
   end First_Index;

   -------------------------------------------------------------------------

   function First_Index (Set : Reader_Condition_Set) return Natural is
   begin
      return Set.Data.First_Index;
   end First_Index;

   -------------------------------------------------------------------------

   function First_Item (Set : Reader_ID_Set) return Reader_ID is
      use type Ada.Containers.Count_Type;
   begin
      if Set.Data.Length = 0 then
         return Null_Reader_ID;
      end if;

      return Set.Data.First_Element;
   end First_Item;

   -------------------------------------------------------------------------

   function First_Item (Set : Reader_States_Set) return Reader_State is
      use type Ada.Containers.Count_Type;
   begin
      return Set.Data.First_Element;
   end First_Item;

   -------------------------------------------------------------------------

   function Get
     (Set   : Reader_ID_Set;
      Index : Natural)
      return Reader_ID
   is
   begin
      --  TODO: bound checks on 'Index'
      return Set.Data.Element (Index);
   end Get;

   -------------------------------------------------------------------------

   function Get
     (Set   : Reader_States_Set;
      Index : Natural)
      return Reader_State
   is
   begin
      --  TODO: bound checks on 'Index'
      return Set.Data.Element (Index);
   end Get;

   -------------------------------------------------------------------------

   function Get
     (Set   : Reader_Condition_Set;
      Index : Natural)
      return Reader_Condition
   is
   begin
      --  TODO: bound checks on 'Index'
      return Set.Data.Element (Index);
   end Get;

   -------------------------------------------------------------------------

   function Get_Active_Proto (Card : SCard.Card) return Proto is
   begin
      return Convert.To_Ada (Card.Active_Proto);
   end Get_Active_Proto;

   -------------------------------------------------------------------------

   procedure Get_Attribute
     (Card        :        SCard.Card;
      Attr        :        Attribute;
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

   -------------------------------------------------------------------------

   function Get_PCI (Card : SCard.Card) return Thin.SCARD_IO_REQUEST
   is
      PCI : Thin.SCARD_IO_REQUEST;
   begin
      --  TODO: use a mapping table for this
      if Card.Active_Proto = Thin.SCARD_PROTOCOL_T0 then
         PCI := Thin.SCARD_PCI_T0;
      elsif Card.Active_Proto = Thin.SCARD_PROTOCOL_T1 then
         PCI := Thin.SCARD_PCI_T1;
      elsif Card.Active_Proto = Thin.SCARD_PROTOCOL_RAW then
         PCI := Thin.SCARD_PCI_RAW;
      else
         raise No_PCI_for_Proto;
      end if;
      return PCI;
   end Get_PCI;

   -------------------------------------------------------------------------

   function Get_Return_Code return String is
      Err_Message : constant String := Strings.Value
        (Thin.pcsc_stringify_error (Last_Return_Code));
   begin
      return Err_Message;
   end Get_Return_Code;

   -------------------------------------------------------------------------

   function Init_Attribute_Set
     (Card : SCard.Card;
      Attr : Attribute)
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

   -------------------------------------------------------------------------

   function Is_In
     (States : Reader_States_Set;
      State  : Reader_State)
      return Boolean
   is
      use type VORSP.Cursor;
   begin
      if States.Data.Find (Item => State) = VORSP.No_Element then
         return False;
      end if;
      return True;
   end Is_In;

   -------------------------------------------------------------------------

   function Is_In
     (States : Card_States_Set;
      State  : Card_State)
      return Boolean
   is
      use type VOCSP.Cursor;
   begin
      if States.Data.Find (Item => State) = VOCSP.No_Element then
         return False;
      end if;
      return True;
   end Is_In;

   -------------------------------------------------------------------------

   function Is_Valid (Context : SCard.Context) return Boolean is
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

   -------------------------------------------------------------------------

   function Last_Index (Set : Reader_ID_Set) return Natural is
   begin
      return Set.Data.Last_Index;
   end Last_Index;

   -------------------------------------------------------------------------

   function Last_Index (Set : Reader_States_Set) return Natural is
   begin
      return Set.Data.Last_Index;
   end Last_Index;

   -------------------------------------------------------------------------

   function Last_Index (Set : Reader_Condition_Set) return Natural is
   begin
      return Set.Data.Last_Index;
   end Last_Index;

   -------------------------------------------------------------------------

   function Last_Item (Set : Reader_ID_Set) return Reader_ID is
      use type Ada.Containers.Count_Type;
   begin
      if Set.Data.Length = 0 then
         return Null_Reader_ID;
      end if;

      return Set.Data.Last_Element;
   end Last_Item;

   -------------------------------------------------------------------------

   function Last_Item (Set : Reader_States_Set) return Reader_State is
      use type Ada.Containers.Count_Type;
   begin
      return Set.Data.Last_Element;
   end Last_Item;

   -------------------------------------------------------------------------

   function Length (Set : Reader_Condition_Set) return Natural is
   begin
      return Natural (Set.Data.Length);
   end Length;

   -------------------------------------------------------------------------

   function List_Readers (Context : SCard.Context) return Reader_ID_Set
   is
      Res : Thin.DWORD;
      Len : aliased Thin.DWORD := 0;
   begin

      --  Calculate the space required to store the reader's friendly name.

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

   -------------------------------------------------------------------------

   procedure Reconnect
     (Card   : in out SCard.Card;
      Mode   :        SCard.Mode;
      Action :        SCard.Action)
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

   -------------------------------------------------------------------------

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

   -------------------------------------------------------------------------

   procedure Remove
     (States : in out Reader_States_Set;
      State  :        Reader_State)
   is
   begin
      --  TODO: error handling when state is not found
      States.Data.Delete (Index => States.Data.Find_Index (Item => State));
   end Remove;

   -------------------------------------------------------------------------

   procedure SCard_Exception
     (Code    : Thin.Return_Code;
      Message : String)
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

   -------------------------------------------------------------------------

   function Size (Atr : SCard.ATR := Null_ATR) return Natural is
   begin
      return Natural (Atr.Length);
   end Size;

   -------------------------------------------------------------------------

   function Size (Atr : SCard.ATR := Null_ATR) return String is
      Natural_ATR : constant Natural := Size (Atr);
   begin

      --  Remove leading space

      return Ada.Strings.Fixed.Trim (Source => Natural'Image (Natural_ATR),
                                     Side   => Ada.Strings.Left);
   end Size;

   -------------------------------------------------------------------------

   procedure SPE_Exec
     (Card   : in out SCard.Card;
      Result : in out Byte_Set)
   is
      package TR renames Thin.Reader;

      Res           : Thin.DWORD;

      Verify_Struct : TR.PIN_VERIFY_STRUCTURE;

      Recv_Buffer   : Thin.Byte_Array (1 .. Thin.MAX_BUFFER_SIZE);
      Recv_Len      : aliased Thin.DWORD := 0;
      Supported     : Boolean            := False;
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
         Result (Index) := Recv_Buffer (Index);
      end loop;
   end SPE_Exec;

   -------------------------------------------------------------------------

   procedure SPE_Init
     (Card   : in out SCard.Card;
      Result : in out Boolean)
   is
      package TR renames Thin.Reader;

      Res         : Thin.DWORD;

      Recv_Buffer : Thin.Byte_Array (1 .. Thin.MAX_BUFFER_SIZE);
      Recv_Len    : aliased Thin.DWORD := 0;
      Elements    : Natural            := 0;
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

         TLV_Array : array (1 .. Elements) of TR.PCSC_TLV_STRUCTURE;

         T         : Natural := Recv_Buffer'First;
         Index     : Natural := TLV_Array'First;

         Value     : Byte_Set (1 .. 4);
      begin
         loop
            exit when T >= Natural (Recv_Len);
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

         for Index in Natural range TLV_Array'Range loop
            if TLV_Array (Index).tag = TR.FEATURE_VERIFY_PIN_DIRECT then

               --  Store verify control code for this card

               Card.Verify_Ctrl := Thin.DWORD (TLV_Array (Index).value);
               Result := True;
               return;
            end if;
         end loop;
      end;
   end SPE_Init;

   -------------------------------------------------------------------------

   procedure Status
     (Card  :     SCard.Card;
      State : out SCard.Card_States_Set;
      Proto : out SCard.Proto;
      Atr   : out SCard.ATR)
   is
      Res         : Thin.DWORD;

      dwReaderLen : aliased Thin.DWORD;
      dwState     : aliased Thin.DWORD;
      dwProtocol  : aliased Thin.DWORD;
      dwAtrLen    : aliased Thin.DWORD := Thin.MAX_ATR_SIZE;
   begin
      Thin.SCardStatus
        (returnValue    => Res,
         hCard          => Card.hCard,
         mszReaderNames => Strings.Null_Ptr,
         pcchReaderLen  => dwReaderLen'Access,
         pdwState       => dwState'Access,
         pdwProtocol    => dwProtocol'Access,
         pbAtr          => Thin.Byte_Array (Atr.Data),
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

   -------------------------------------------------------------------------

   procedure Status_Change
     (Context    :        SCard.Context;
      Timeout    :        Natural := 0;
      Conditions : in out Reader_Condition_Set)
   is
      use type VORCP.Vector;

      Res       : Thin.DWORD;
      C_Timeout : Thin.DWORD;
      C_States  : Thin.READERSTATE_Array := Convert.To_C
        (Conditions => Conditions);

      procedure Update_Status_Set (Position : VORCP.Cursor) is

         procedure Update_Reader_Condition (Element : in out Reader_Condition)
         is
            use type Interfaces.Unsigned_64;

            Counter : Interfaces.Unsigned_64;
         begin
            Element.Event_State     := Convert.To_Ada
              (C_States (size_t (VORCP.To_Index (Position))).dwEventState);
            Element.Card_ATR.Data   := ATR_Type
              (C_States (size_t (VORCP.To_Index (Position))).rgbAtr);
            Element.Card_ATR.Length := ATR_Index
              (C_States (size_t (VORCP.To_Index (Position))).cbAtr);

            --  Update event counter for this condition

            Counter := Interfaces.Unsigned_64
              (C_States (size_t (VORCP.To_Index (Position))).dwEventState);
            Counter := Interfaces.Shift_Right (Value  => Counter,
                                               Amount => 16);
            Counter := Counter and 16#FFFF#;
            Element.Event_Counter := Natural (Counter);
         end Update_Reader_Condition;

      begin
         Conditions.Data.Update_Element
           (Position => Position,
            Process  => Update_Reader_Condition'Access);
      end Update_Status_Set;

   begin

      --  Check for empty reader conditions

      if Conditions.Data = VORCP.Empty_Vector then

         --  Just wait for readers to appear, then return

         Wait_For_Readers (Context => Context);
         return;
      end if;

      if Timeout = 0 then
         C_Timeout := Thin.INFINITE;
      end if;

      if Conditions.Data = VORCP.Empty_Vector then
         return;
      else
         Res := Thin.SCardGetStatusChange
           (hContext       => Context.hContext,
            dwTimeout      => C_Timeout,
            rgReaderStates => C_States (C_States'First)'Access,
            cReaders       => Thin.DWORD (C_States'Last));
      end if;

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Status change detection failed");
      end if;
      Store_Error (Code => Res);

      --  Update Ada type with values returned by C API function

      VORCP.Iterate (Container => Conditions.Data,
                     Process   => Update_Status_Set'Access);

      --  Free C_States

      Convert.Free (Name => C_States);

   end Status_Change;

   -------------------------------------------------------------------------

   procedure Store_Error (Code : Thin.Return_Code) is
   begin
      Last_Return_Code := Code;
   end Store_Error;

   -------------------------------------------------------------------------

   function To_Atr (Bytes : Byte_Set) return ATR is
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

   -------------------------------------------------------------------------

   function To_Reader_ID (Name : String) return Reader_ID is
   begin
      return Reader_ID'(To_Unbounded_String (Name));
   end To_Reader_ID;

   -------------------------------------------------------------------------

   procedure Transmit
     (Card        :        SCard.Card;
      Send_Buffer :        Byte_Set := Null_Byte_Set;
      Recv_Pci    : in out IO_Request;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural)
   is
      Res            : Thin.DWORD;

      C_Send_PCI     : aliased Thin.SCARD_IO_REQUEST;
      C_Recv_PCI     : aliased Thin.SCARD_IO_REQUEST := Recv_Pci;
      Bytes_Returned : aliased Thin.DWORD            :=
        Thin.DWORD (Recv_Buffer'Last);
   begin

      --  Empty send buffer makes no sense, return without doing anything

      if Send_Buffer = Null_Byte_Set then
         return;
      end if;

      --  Assign correct send PCI depending on active proto of card

      C_Send_PCI := Get_PCI (Card => Card);

      --  Call thin binding SCardTransmit

      Res := Thin.SCardTransmit
        (hCard         => Card.hCard,
         pioSendPci    => C_Send_PCI'Access,
         pbSendBuffer  => Thin.Byte_Array (Send_Buffer),
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

   -------------------------------------------------------------------------

   procedure Wait_For_Readers (Context : SCard.Context) is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardGetStatusChange
        (hContext       => Context.hContext,
         dwTimeout      => Thin.INFINITE,
         rgReaderStates => null);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Waiting for readers failed");
      end if;
      Store_Error (Code => Res);
   end Wait_For_Readers;

end PCSC.SCard;
