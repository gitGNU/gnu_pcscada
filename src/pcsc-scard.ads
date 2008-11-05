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
--    PC/SC thick-binding package. Provides abstraction from the C style
--    functions and types provided in the thin binding in @PCSC.Thin@.
--  </PURPOSE>
--

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with PCSC.Thin;

package PCSC.SCard is

   type Context is limited private;
   --  This is the PC/SC-Context. This object is initialized by
   --  Establish_Context below and is used with all services.

   type Card is limited private;
   --  SCard-Handler, returned by Connect. Used to access a specific smartcard.


   type Byte_Set is array (Natural range <>) of aliased Thin.Byte;
   --  Byte_Sets are used to send and receive data to/from SCard-Readers.

   Null_Byte_Set : constant Byte_Set;
   --  Empty Byte_Set


   type ATR is private;
   --  Card ATR type

   Null_ATR : constant ATR;
   --  Null initialized ATR


   type Reader_ID is private;
   --  Reader friendly name

   Null_Reader_ID : constant Reader_ID;
   --  Empty reader ID

   type Reader_ID_Set is tagged private;
   --  Set of ReaderIDs


   type Scope is
     (Scope_User,
      --  Scope in user space
      Scope_Terminal,
      --  Scope in terminal
      Scope_System
      --  Scope in system
     );
   --  Possible scopes for a PC/SC-context

   type Mode is
     (Share_Exclusive,
      --  Exclusive mode only
      Share_Shared,
      --  Shared mode only
      Share_Direct
      --  Raw mode only
     );
   --  Possible share Modes for SCard connects

   type Proto is
     (Proto_Undefined,
      --  Protocol not set
      Proto_Unset,
      --  Backward compatibility
      Proto_T0,
      --  T=0 active protocol
      Proto_T1,
      --  T=1 active protocol
      Proto_RAW,
      --  Raw active protocol
      Proto_T15
      --  T=15 protocol
     );
   --  Possible Protos for SCard connects

   type Action is
     (Leave_Card,
      --  Do nothing on close
      Reset_Card,
      --  Reset on close
      Unpower_Card,
      --  Power down on close
      Eject_Card
      --  Eject on close
     );
   --  Possible card Actions

   type Card_State is
     (S_Card_Unknown,
      --  Unknown state
      S_Card_Absent,
      --  Card is absent
      S_Card_Present,
      --  Card is present
      S_Card_Swallowed,
      --  Card not powered
      S_Card_Powered,
      --  Card is powered
      S_Card_Negotiable,
      --  Ready for PTS
      S_Card_Specific
      --  PTS has been set
     );
   --  Card states

   type Card_States_Set is tagged private;
   --  Set of card states

   type Reader_State is
     (S_Reader_Unaware,
      --  App wants status
      S_Reader_Ignore,
      --  Ignore this reader
      S_Reader_Changed,
      --  State has changed
      S_Reader_Unknown,
      --  Reader unknown
      S_Reader_Unavailable,
      --  Status unavailable
      S_Reader_Empty,
      --  Card removed
      S_Reader_Present,
      --  Card inserted
      S_Reader_Atrmatch,
      --  ATR matches card
      S_Reader_Exclusive,
      --  Exclusive Mode
      S_Reader_Inuse,
      --  Shared Mode
      S_Reader_Mute,
      --  Unresponsive card
      S_Reader_Unpowered
      --  Unpowered card
     );
   --  Reader / Card states

   type Reader_States_Set is tagged private;
   --  Set of reader states


   type Reader_Condition is record
      Name          : Reader_ID := Null_Reader_ID;
      Current_State : Reader_State;
      Event_State   : Reader_States_Set;
      Card_ATR      : ATR := Null_ATR;
   end record;
   --  Reader condition type for status change handling. Current_State defines
   --  the current assumed state. Event_State and Card_ATR is updated by
   --  calling the Status_Change() procedure.

   type Reader_Condition_Set is tagged private;
   --  Set of reader status types


   type PCI is
     (PCI_T0,
      --  (PCI) for T=0
      PCI_T1,
      --  (PCI) for T=1
      PCI_RAW
      --  (PCI) for RAW protocol
     );
   --  Protocol control information types


   type Attribute is
     (Attr_Vendor_Name,
      Attr_Vendor_IFD_Type,
      Attr_Vendor_IFD_Version,
      Attr_Vendor_IFD_Serial,
      Attr_Channel_ID,
      Attr_Default_CLK,
      Attr_Max_CLK,
      Attr_Default_Data_Rate,
      Attr_Max_Data_Rate,
      Attr_Max_IFSD,
      Attr_Power_Mgmt_Support,
      Attr_Characteristics,
      Attr_Current_Protocol_Type,
      Attr_Current_CLK,
      Attr_Current_F,
      Attr_Current_D,
      Attr_Current_N,
      Attr_Current_W,
      Attr_Current_IFSC,
      Attr_Current_IFSD,
      Attr_Current_BWT,
      Attr_Current_CWT,
      Attr_Current_EBC_Encoding,
      Attr_Extended_BWT,
      Attr_ICC_Presence,
      Attr_ICC_Interface_Status,
      Attr_Current_IO_State,
      Attr_ATR_String,
      Attr_ICC_Type_Per_ATR,
      Attr_ESC_Preset,
      Attr_ESC_Cancel,
      Attr_ESC_Authrequest,
      Attr_Maxinput,
      Attr_Device_Unit,
      Attr_Device_In_Use,
      Attr_Device_Friendly_Name_A,
      Attr_Device_System_Name_A,
      Attr_Device_Friendly_Name_W,
      Attr_Device_System_Name_W,
      Attr_Supress_T1_IFS_Request);
   --  Possible attributes for Get_Attribute procedure


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
                          return Reader_ID_Set;
   --  Return list of all available readers for this PC/SC context.

   procedure Status_Change
     (Context    : in SCard.Context;
      Timeout    : in Natural := 0;
      Status_Set : in out Reader_Condition_Set);
   --  This procedure takes a Reader_Condition_Set type containing reader names
   --  and assumed initial state. It then blocks maximum 'Timeout' milliseconds
   --  time for a change in state to occur. If no timeout is given, 0 will be
   --  used, which will block forever. When a status change occurs, the
   --  Reader_States type is updated to reflect this new state.

   procedure Wait_For_Readers (Context : in SCard.Context);
   --  This procedure calls SCardGetStatusChange for reader detection. If there
   --  is no reader available, the call will block until a reader is connected.
   --  If there are readers present when this function is called, it will
   --  return immediately.

   procedure Connect
     (Context : in SCard.Context;
      Card    : in out SCard.Card;
      Reader  : in Reader_ID := Null_Reader_ID;
      Mode    : in SCard.Mode := Share_Shared);
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
     (Card  : in SCard.Card;
      State : in out SCard.Card_States_Set;
      Proto : in out SCard.Proto;
      Atr   : in out SCard.ATR);
   --  This procedure checks the current status of the reader connected to by
   --  'Card'. Current state, protocol and ATR value of inserted card are
   --  returned as in out params.

   procedure Transmit
     (Card        : in SCard.Card;
      Send_Pci    : in PCI;
      Send_Buffer : in Byte_Set := Null_Byte_Set;
      Recv_Pci    : in PCI;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural);
   --  Transmit APDUs to SCard.

   procedure Control
     (Card        : in SCard.Card;
      Code        : in Natural;
      Send_Buffer : in Byte_Set := Null_Byte_Set;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural);
   --  This procedure sends a control command to the reader connected to by
   --  Connect(). It returns the the control response in Recv_Buffer.

   procedure Get_Attribute
     (Card        : in SCard.Card;
      Attr        : in Attribute;
      Recv_Buffer : in out Byte_Set);
   --  This procedure gets an attribute from the IFD handler. Call
   --  Init_Attribute_Set function to allocate the correct byte set for the
   --  call.

   function Init_Attribute_Set
     (Card        : in SCard.Card;
      Attr        : in Attribute)
      return Byte_Set;
   --  This function returns an Null_Byte initialized byte set to provide
   --  storage for an attribute 'Attr'. The byte set can then be used as
   --  parameter to the Get_Attribute function to actually retrieve the
   --  attribute data.

   function Get_Active_Proto (Card : in SCard.Card) return Proto;
   --  Return protocol in use for a given card handle.


   procedure SPE_Init (Card : in out SCard.Card; Result : in out Boolean);
   --  Procedure checks if a reader connected by 'Card' handle supports SPE
   --  (secure PIN entry) operation. If it does, 'Result' will be True,
   --  otherwise 'Result' will be False.

   procedure SPE_Exec (Card : in out SCard.Card; Result : in out Byte_Set);
   --  Request the reader connected to by 'Card' to perform a secure PIN entry
   --  operation. If not already done by the client code, this procedure will
   --  use the SPE_Init() procedure to check if the reader supports SPE and to
   --  initialize the verify ctrl code. If the SPE operation is not supported
   --  or does not work as expected, an SCard_Not_Supported exception is
   --  raised. If the SPE operation is successful, the bytes returned from the
   --  reader will be stored in 'Result' byte set.


   function To_Reader_ID (Name : in String) return Reader_ID;
   --  Return a new Reader_ID object initialized with 'Name' string.

   function First (Set : in Reader_ID_Set) return Reader_ID;
   --  Return the first reader in a reader ID set.

   function Last (Set : in Reader_ID_Set) return Reader_ID;
   --  Return the last reader in a reader ID set.

   function Get (Set : in Reader_ID_Set; Index : in Natural) return Reader_ID;
   --  Return Reader_ID object at index 'Index'.

   function Empty (Set : in Reader_ID_Set) return Boolean;
   --  Function returns true if Reader_ID_Set contains no readers.


   procedure Add (States : in out Reader_States_Set; State : in Reader_State);
   --  Add a new reader state to Reader_States_Set.

   function Is_In
     (States : in Reader_States_Set;
      State  : in Reader_State)
      return Boolean;
   --  Function returns True if given Reader_State 'State' is found in Reader
   --  states set 'States'.

   procedure Add (States : in out Card_States_Set; State : in Card_State);
   --  Add a new card state to Card_States_Set.

   function Is_In
     (States : in Card_States_Set;
      State  : in Card_State)
      return Boolean;
   --  Function returns True if given Card_State 'State' is found in Card
   --  states set 'States'.


   procedure Add
     (Set    : in out Reader_Condition_Set;
      Status : in Reader_Condition);
   --  Add a new reader to reader condition array. State specifies the assumed
   --  initial state of the reader/card.

   function Size (Set : in Reader_Condition_Set) return Natural;
   --  Returns the size of a Reader_Condition_Set.

   function Get (Set    : in Reader_Condition_Set;
                 Index  : in Natural)
                 return Reader_Condition;
   --  Return Reader_Condition object at index 'Index'.


   function To_Atr (Bytes : in Byte_Set) return ATR;
   --  Create new ATR object from given Byte_Set. If 'Bytes is too big to be
   --  converted into an ATR type, a 'Bytes_Too_Big' exception will be raised.

   function Size (Atr : in SCard.ATR := Null_ATR) return Natural;
   --  Return current size of an ATR as Natural.

   function Size (Atr : in SCard.ATR := Null_ATR) return String;
   --  Return current size of an ATR as string.


   function Get_Return_Code return String;
   --  Return string representation of last stored return code.

private

   procedure SCard_Exception (Code : in Thin.Return_Code; Message : in String);
   pragma No_Return (SCard_Exception);
   --  Raise SCard exception if something goes wrong.

   Last_Return_Code : Thin.Return_Code;
   --  Holds the return code of the last SCard operation. Get_Return_Code
   --  can be used to obtain this code by the client. Store_Error should be
   --  used to save the return code after a thin binding call.

   procedure Store_Error (Code : in Thin.Return_Code);
   --  Saves the last returned code.


   type Context is limited record
      hContext : aliased Thin.SCARDCONTEXT;
   end record;

   type Card is limited record
      hCard        : aliased Thin.SCARDHANDLE;
      --  Card handle used by the thin binding
      Active_Proto : aliased Thin.DWORD := Thin.SCARD_PROTOCOL_UNDEFINED;
      --  Current protocol used by the card
      Verify_Ctrl  : Thin.DWORD := 0;
      --  Verify control code needed to perform SPE
   end record;


   Null_Byte_Set : constant Byte_Set (1 .. 0) := (others => Thin.Null_Byte);


   subtype ATR_Index is Natural range 0 .. Thin.MAX_ATR_SIZE;
   --  Allowed index values for valid ATRs. '0' is used to indicate Null_ATR.

   type ATR_Type is new Byte_Set (ATR_Index'Range);
   --  ATR type definition. Defines a constrained Byte_Set to store ATRs.

   type ATR is record
      Data   : ATR_Type;
      --  ATR data bytes
      Length : ATR_Index;
      --  Bytes count
   end record;

   Null_ATR : constant ATR := ATR'(Data   => ATR_Type'
                                     (others => Thin.Null_Byte),
                                   Length => 0);


   --  Reader IDs

   type Reader_ID is new Unbounded_String;

   Null_Reader_ID : constant Reader_ID := Reader_ID (Null_Unbounded_String);

   package Vector_Of_ReaderID_Package is new
     Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                        Element_Type => Reader_ID);

   package VOIDP renames Vector_Of_ReaderID_Package;
   subtype Vector_Of_ReaderID_Type is VOIDP.Vector;

   type Reader_ID_Set is tagged record
      Data : Vector_Of_ReaderID_Type;
   end record;


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
                                        Element_Type => Reader_Condition);

   package VORSTP renames Vector_Of_Status_Package;
   subtype Vector_Of_Status_Type is VORSTP.Vector;

   type Reader_Condition_Set is tagged record
      Data : Vector_Of_Status_Type;
   end record;

end PCSC.SCard;
