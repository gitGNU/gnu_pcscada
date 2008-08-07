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

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with PCSC.Thin;

package PCSC.SCard is

   package IC renames Interfaces.C;

   type Context is limited private;
   --  This is the PC/SC-Context. This object is initialized by
   --  Establish_Context below and is used with all services.

   type Card is limited private;
   --  SCard-Handler, returned by Connect. Used to access a specific smartcard.

   type Scope is
     (Scope_User,     --  Scope in user space
      Scope_Terminal, --  Scope in terminal
      Scope_System);  --  Scope in system
   --  Possible scope for PC/SC-context

   type Mode is
     (Share_Exclusive, -- Exclusive mode only
      Share_Shared,    -- Shared mode only
      Share_Direct);   -- Raw mode only
   --  Possible Mode for SCard connects

   type Proto is
     (Proto_Undefined, --  Protocol not set
      Proto_Unset,     --  Backward compatibility
      Proto_T0,        --  T=0 active protocol
      Proto_T1,        --  T=1 active protocol
      Proto_RAW,       --  Raw active protocol
      Proto_T15);      --  T=15 protocol
   --  Possible Protos for SCard connects

   type Action is
     (Leave_Card,   --  Do nothing on close
      Reset_Card,   --  Reset on close
      Unpower_Card, --  Power down on close
      Eject_Card);  --  Eject on close
   --  Desired action taken on the card/reader

   type Card_State is
     (Unknown,    -- Unknown state
      Absent,     -- Card is absent
      Present,    -- Card is present
      Swallowed,  -- Card not powered
      Powered,    -- Card is powered
      Negotiable, -- Ready for PTS
      Specific);  -- PTS has been set
   --  Card states

   type Card_States is tagged private;
   --  Card states handling.

   type Reader_State is
     (State_Unaware,     --  App wants status
      State_Ignore,      --  Ignore this reader
      State_Changed,     --  State has changed
      State_Unknown,     --  Reader unknown
      State_Unavailable, --  Status unavailable
      State_Empty,       --  Card removed
      State_Present,     --  Card inserted
      State_Atrmatch,    --  ATR matches card
      State_Exclusive,   --  Exclusive Mode
      State_Inuse,       --  Shared Mode
      State_Mute,        --  Unresponsive card
      State_Unpowered);  --  Unpowered card
   --  Reader / Card states

   type PCI is
     (PCI_T0,   --  (PCI) for T=0
      PCI_T1,   --  (PCI) for T=1
      PCI_RAW); --  (PCI) for RAW protocol
   --  Protocol control information types

   subtype Reader_ID is Unbounded_String;
   --  Reader friendly name

   package Readers_Vector is new
     Ada.Containers.Indefinite_Vectors (Positive, Reader_ID);
   use Readers_Vector;
   --  Vector of readers

   subtype Readers_List is Readers_Vector.Vector;
   --  Readers list returned by List_Readers()

   subtype ATR is Thin.ATR;
   ATR_Length : constant Integer := Thin.MAX_ATR_SIZE;
   --  Card ATR. Directly mapped to thin binding ATR (atm), which is a
   --  Byte_Array type.

   type Callback is access procedure (ID : in Reader_ID);
   --  Callback for reader ID handling. Provides flexible way to access
   --  specific readers.

   procedure Establish_Context
     (Context : in out SCard.Context;
      Scope   : in SCard.Scope);
   --  Establish PC/SC-context. Possible Scope values are defined in
   --  SCard_Scope type.

   procedure Release_Context (Context : in out SCard.Context);
   --  Release acquired SCard-context.

   function Is_Valid (Context : in SCard.Context) return Boolean;
   --  Verify that given SCard-Context is valid.

   function List_Readers (Context : in SCard.Context)
                          return Readers_List;
   --  Return list of all available readers for this PC/SC context.

   procedure Connect
     (Card    : in out SCard.Card;
      Context : in SCard.Context;
      Reader  : in Reader_ID;
      Mode    : in SCard.Mode);
   --  Connect to a SCard identified by Reader (Reader_ID). Handle to connected
   --  SCard will be stored in 'Card' parameter.

   procedure Disconnect (Card   : in SCard.Card; Action : in SCard.Action);
   --  This procedure terminates a connection to the connection made through
   --  Connect procedure.

   procedure Reconnect
     (Card   : in out SCard.Card;
      Mode   : in SCard.Mode;
      Action : in SCard.Action);
   --  This procedure reestablishes a connection to a reader that was
   --  previously connected to using Connect(). Init defines the desired action
   --  taken on the card/reader.

   procedure Begin_Transaction (Card : in SCard.Card);
   --  This procedure establishes a temporary exclusive access mode for doing
   --  a series of commands or transactions with a SCard.

   procedure End_Transaction
     (Card   : in SCard.Card;
      Action : in SCard.Action);
   --  This procedure ends a previously begun transaction.

   procedure Status
     (Card    : in SCard.Card;
      State   : in out SCard.Card_States;
      Proto   : in out SCard.Proto;
      Atr     : in out SCard.ATR;
      Atr_Len : in out Integer);
   --  This procedure checks the current status of the reader connected to by
   --  'Card'. Current state, protocol and ATR value of inserted card are
   --  returned as in out params.

   procedure Transmit
     (Card        : in SCard.Card;
      Send_Pci    : in PCI;
      Send_Buffer : in out Thin.Byte_Array;
      Recv_Pci    : in PCI;
      Recv_Buffer : in out Thin.Byte_Array;
      Recv_Len    : in out Natural);
   --  Transmit APDUs to SCard.

   function Get_Active_Proto (Card : in SCard.Card) return Proto;
   --  Return protocol in use for a given card handle.


   --  Lookup functions. Used to get Ada type from Thin.DWORD value.

   function To_LPSTR (Reader : in Reader_ID) return IC.Strings.chars_ptr;
   --  Return a new C compatible string from Reader_ID. The allocated memory
   --  must be freed by calling Free.

   function To_Ada (C_Protocol : Thin.DWORD) return Proto;
   --  Return Ada style Proto for C_Protocol (DWORD).

   function To_Ada (C_State : Thin.DWORD) return Card_States;
   --  Return Ada style Card_States for C_State (DWORD).

private

   function Slice_Readerstring (To_Slice : in String) return Readers_List;
   --  Slice reader string returned from thin binding and create vector of
   --  reader names. The string to slice has a format like:
   --  Reader A1\0Reader B1\0Reader C1\0\0
   --  \0 is used as separator, \0\0 as string termination.

   procedure SCard_Exception (Code : in Thin.Return_Code; Message : in String);
   pragma No_Return (SCard_Exception);
   --  Raise SCard exception if something goes wrong.

   type Context is limited record
      hContext : aliased Thin.SCARDCONTEXT;
   end record;

   type Card is limited record
      hCard        : aliased Thin.SCARDHANDLE;
      Active_Proto : aliased Thin.DWORD := Thin.SCARD_PROTOCOL_UNDEFINED;
   end record;

   --  Card states, wrapper for indef vector

   package Vector_Of_States_Package is new
     Ada.Containers.Indefinite_Vectors (Index_Type   => Natural,
                                        Element_Type => Card_State);

   package VOSP renames Vector_Of_States_Package;
   subtype Vector_Of_States_Type is VOSP.Vector;

   type Card_States is tagged record
      Data : Vector_Of_States_Type;
   end record;

end PCSC.SCard;
