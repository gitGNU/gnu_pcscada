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
with Ada.Characters.Latin_1;

with GNAT.String_Split;

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
     := (Unknown    => Thin.SCARD_UNKNOWN,
         Absent     => Thin.SCARD_ABSENT,
         Present    => Thin.SCARD_PRESENT,
         Swallowed  => Thin.SCARD_SWALLOWED,
         Powered    => Thin.SCARD_POWERED,
         Negotiable => Thin.SCARD_NEGOTIABLE,
         Specific   => Thin.SCARD_SPECIFIC);
   --  Map Card_State to corresponding C values.

   C_Reader_State : constant array (Reader_State) of Thin.DWORD
     := (State_Unaware     => Thin.SCARD_STATE_UNAWARE,
         State_Ignore      => Thin.SCARD_STATE_IGNORE,
         State_Changed     => Thin.SCARD_STATE_CHANGED,
         State_Unknown     => Thin.SCARD_STATE_UNKNOWN,
         State_Unavailable => Thin.SCARD_STATE_UNAVAILABLE,
         State_Empty       => Thin.SCARD_STATE_EMPTY,
         State_Present     => Thin.SCARD_STATE_PRESENT,
         State_Atrmatch    => Thin.SCARD_STATE_ATRMATCH,
         State_Exclusive   => Thin.SCARD_STATE_EXCLUSIVE,
         State_Inuse       => Thin.SCARD_STATE_INUSE,
         State_Mute        => Thin.SCARD_STATE_MUTE,
         State_Unpowered   => Thin.SCARD_STATE_UNPOWERED);
   --  Map Reader_State to corresponding C values

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
   end Release_Context;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Context : in SCard.Context) return Boolean is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardIsValidContext (hContext => Context.hContext);

      if Res /= Thin.SCARD_S_SUCCESS then
         return False;
      end if;

      return True;
   end Is_Valid;

   ------------------
   -- List_Readers --
   ------------------

   function List_Readers (Context : in SCard.Context)
                          return Readers_List
   is
      Res       : Thin.DWORD;
      Len       : aliased Thin.DWORD;
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
         C_Readers : aliased char_array := (1 .. IC.size_t (Len) => <>);
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

         --  Convert to Ada types

         declare
            Readers : String := To_Ada (Item     => C_Readers,
                                        Trim_Nul => False);
         begin
            return Slice_Readerstring (To_Slice => Readers);
         end;
      end;
   end List_Readers;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Card    : in out SCard.Card;
      Context : in SCard.Context;
      Reader  : in Reader_ID;
      Mode    : in SCard.Mode)
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
   end End_Transaction;

   ------------
   -- Status --
   ------------

   procedure Status
     (Card    : in SCard.Card;
      State   : in out SCard.Card_States;
      Proto   : in out SCard.Proto;
      Atr     : in out SCard.ATR;
      Atr_Len : in out Integer)
   is
      Res         : Thin.DWORD;

      dwReaderLen : aliased Thin.DWORD;
      dwState     : aliased Thin.DWORD;
      dwProtocol  : aliased Thin.DWORD;
      dwAtrLen    : aliased Thin.DWORD := Thin.MAX_ATR_SIZE;
   begin
      Res := Thin.SCardStatus
        (hCard          => Card.hCard,
         mszReaderNames => IC.Strings.Null_Ptr,
         pcchReaderLen  => dwReaderLen'Access,
         pdwState       => dwState'Unchecked_Access,
         pdwProtocol    => dwProtocol'Access,
         pbAtr          => Atr (Atr'First)'Unchecked_Access,
         pcbAtrLen      => dwAtrLen'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Get status failed");
      end if;

      --  Assign in out params

      Atr_Len := Integer (dwAtrLen);
      Proto   := To_Ada (dwProtocol);
      State   := To_Ada (dwState);
   end Status;

   --------------
   -- Transmit --
   --------------

   procedure Transmit
     (Card        : in SCard.Card;
      Send_Pci    : access Thin.SCARD_IO_REQUEST;
      Send_Buffer : in out Thin.Byte_Array;
      Recv_Pci    : access Thin.SCARD_IO_REQUEST;
      Recv_Buffer : in out Thin.Byte_Array)
   is
      Res         : Thin.DWORD;

      Recv_Length : aliased Thin.DWORD := Thin.DWORD (Recv_Buffer'Last);
   begin
      Res := Thin.SCardTransmit
        (hCard         => Card.hCard,
         pioSendPci    => Send_Pci,
         pbSendBuffer  => Send_Buffer (Send_Buffer'First)'Unchecked_Access,
         cbSendLength  => Thin.DWORD (Send_Buffer'Length),
         pioRecvPci    => Recv_Pci,
         pbRecvBuffer  => Recv_Buffer (Recv_Buffer'First)'Unchecked_Access,
         pcbRecvLength => Recv_Length'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Transmit failed");
      end if;
   end Transmit;

   ----------------------
   -- Get_Active_Proto --
   ----------------------

   function Get_Active_Proto (Card : in SCard.Card) return Proto is
   begin
      return To_Ada (Card.Active_Proto);
   end Get_Active_Proto;

   ---------------------
   -- SCard_Exception --
   ---------------------

   procedure SCard_Exception (Code : in Thin.Return_Code; Message : in String)
   is
      Err_Message : constant String := Strings.Value
        (Thin.pcsc_stringify_error (Code));
   begin
      Ada.Exceptions.Raise_Exception
        (SCard_Error'Identity,
         Message & " - ["
           & Thin.DWORD'Image (Code) & "] " & Err_Message);
   end SCard_Exception;

   ------------------------
   -- Slice_Readerstring --
   ------------------------

   function Slice_Readerstring (To_Slice : in String) return Readers_List
   is
      use GNAT.String_Split;
      use Ada.Strings.Maps;

      Readers  : Readers_List;
      Lines    : Slice_Set;
   begin
      --  Slice readers into parts.
      --  Who uses '\0' as separator anyway?

      Create
        (S          => Lines,
         From       => To_Slice (To_Slice'First .. To_Slice'Last),
         Separators => To_Set (Ada.Characters.Latin_1.NUL),
         Mode       => Single);

      --  Minus two because \0\0 is used as termination.

      for J in 1 .. Slice_Count (Lines) - 2 loop
         Readers.Append (New_Item => To_Unbounded_String
                         (Slice (Lines, J)));
      end loop;

      return Readers;

   end Slice_Readerstring;

   --------------
   -- To_LPSTR --
   --------------

   function To_LPSTR (Reader : in Reader_ID) return IC.Strings.chars_ptr is
   begin
      return Strings.New_String (To_String (Reader));
   end To_LPSTR;

   --------------------------
   -- To_Ada (SCard_Proto) --
   --------------------------

   function To_Ada (C_Protocol : Thin.DWORD) return Proto is
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
   -- To_Ada (SCard_State_Array) --
   --------------------------------

   function To_Ada (C_State : Thin.DWORD) return Card_States is
      States     : Card_States;
   begin
      for P in C_Card_State'Range loop
         if (C_State and C_Card_State (P)) /= 0 then
            States.Data.Append (New_Item => P);
         end if;
      end loop;
      return States;
   end To_Ada;

end PCSC.SCard;
