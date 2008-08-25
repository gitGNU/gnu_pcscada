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

--  PC/SC thick-binding package
package PCSC.SCard is

   package IC renames Interfaces.C;

   type Context is limited private;
   --  This is the PC/SC-Context. This object is initialized by
   --  Establish_Context below and is used with all services.

   type Card is limited private;
   --  SCard-Handler, returned by Connect. Used to access a specific smartcard.


   subtype Byte_Set is Thin.Byte_Array;
   --  Byte_Sets are used to send and receive data to/from SCard-Readers. At
   --  the moment, Byte_Sets are just subtypes of Thin.Byte_Arrays.

   Null_Byte_Set : constant Byte_Set;
   --  Empty Byte_Set


   type ATR is private;
   --  Card ATR type

   Null_ATR : constant ATR;
   --  Null initialized ATR


   subtype Reader_ID is Unbounded_String;
   --  Reader friendly name

   package Readers_Vector is new
     Ada.Containers.Indefinite_Vectors (Positive, Reader_ID);
   --  Vector of readers

   subtype Readers_List is Readers_Vector.Vector;
   --  Readers list returned by List_Readers()


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

   type Card_States_Set is tagged private;
   --  Set of card states

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

   type Reader_States_Set is tagged private;
   --  Set of reader states


   type Reader_Status is record
      Name          : Reader_ID;
      Current_State : Reader_State;
      Event_State   : Reader_States_Set;
      Card_ATR      : ATR := Null_ATR;
   end record;
   --  Reader status type for status change handling. Current_State defines
   --  the current assumed state. Event_State and Card_ATR is updated by
   --  calling Status_Change procedure.

   type Reader_Status_Set is tagged private;
   --  Set of reader status types


   type PCI is
     (PCI_T0,   --  (PCI) for T=0
      PCI_T1,   --  (PCI) for T=1
      PCI_RAW); --  (PCI) for RAW protocol
   --  Protocol control information types

   type Callback is access procedure (ID : in Reader_ID);
   --  Callback for reader ID handling. Provides flexible way to access
   --  specific readers.

   procedure Establish_Context
     (Context : in out SCard.Context;
      Scope   : in SCard.Scope);
   --  Establish PC/SC-context. Possible Scope values are defined in
   --  SCard_Scope type.

   procedure Release_Context (Context : in out SCard.Context);
   --  Release previously acquired SCard context.

   function Is_Valid (Context : in SCard.Context) return Boolean;
   --  Verify that given SCard context is valid.

   function List_Readers (Context : in SCard.Context)
                          return Readers_List;
   --  Return list of all available readers for this PC/SC context.

   procedure Status_Change
     (Context    : in SCard.Context;
      Timeout    : in Natural := 0;
      Status_Set : in out Reader_Status_Set);
   --  This procedure takes a Reader_Status_Set type containing reader names
   --  and assumed initial state. It then blocks maximum 'Timeout' miliseconds
   --  time for a change in state to occur. If no timeout is given, 0 will be
   --  used, which will block forever. When a status change occurs, the
   --  Reader_States type is updated to reflect this new state.

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
      State   : in out SCard.Card_States_Set;
      Proto   : in out SCard.Proto;
      Atr     : in out SCard.ATR);
   --  This procedure checks the current status of the reader connected to by
   --  'Card'. Current state, protocol and ATR value of inserted card are
   --  returned as in out params.

   procedure Transmit
     (Card        : in SCard.Card;
      Send_Pci    : in PCI;
      Send_Buffer : in out Byte_Set;
      Recv_Pci    : in PCI;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural);
   --  Transmit APDUs to SCard.

   function Get_Active_Proto (Card : in SCard.Card) return Proto;
   --  Return protocol in use for a given card handle.


   procedure Add_Reader
     (States : in out Reader_Status_Set;
      State  : in Reader_Status);
   --  Add a new reader to reader status array. State specifies the assumed
   --  initial state of the reader/card.

   function Size (States : in Reader_Status_Set) return Natural;
   --  Returns the size of a Reader_Status_Set.

   function Get_Status (States : in Reader_Status_Set;
                        Index  : Natural)
                        return Reader_Status;
   --  Return Reader_Status type at index 'Index'.

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


   Null_Byte : constant Thin.Byte := 16#00#;

   Null_Byte_Set : constant Byte_Set (1 .. 0)
     := (others => Null_Byte);


   type ATR_Range is range 0 .. Thin.MAX_ATR_SIZE;
   --  Allowed length for valid ATRs. '0' is used to indicate Null_ATR.

   type ATR is record
      Data   : Thin.ATR;
      Length : ATR_Range;
   end record;

   Null_ATR : constant ATR := ATR'(Data   => Thin.Null_ATR,
                                   Length => 0);


   --  Card states

   package Vector_Of_CStates_Package is new
     Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                        Element_Type => Card_State);

   package VOCSP renames Vector_Of_CStates_Package;
   subtype Vector_Of_CStates_Type is VOCSP.Vector;

   type Card_States_Set is tagged record
      Data : Vector_Of_CStates_Type;
   end record;

   --  Reader states

   package Vector_Of_RStates_Package is new
     Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                        Element_Type => Reader_State);

   package VORSP renames Vector_Of_RStates_Package;
   subtype Vector_Of_RStates_Type is VORSP.Vector;

   type Reader_States_Set is tagged record
      Data : Vector_Of_RStates_Type;
   end record;

   --  Reader status

   package Vector_Of_Status_Package is new
     Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                        Element_Type => Reader_Status);

   package VORSTP renames Vector_Of_Status_Package;
   subtype Vector_Of_Status_Type is VORSTP.Vector;

   type Reader_Status_Set is tagged record
      Data : Vector_Of_Status_Type;
   end record;

end PCSC.SCard;
