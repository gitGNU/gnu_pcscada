--
--  Copyright (c) 2008-2009,
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
      Current_State : Reader_States_Set;
      Event_State   : Reader_States_Set;
      Event_Counter : Natural := 0;
      Card_ATR      : ATR     := Null_ATR;
   end record;
   --  Reader condition type for status change handling. Current_State defines
   --  the current assumed state. Event_State and Card_ATR is updated by
   --  calling the Status_Change() procedure. Event_Counter stores the event
   --  count for the reader identified by 'Name'.

   type Reader_Condition_Handle is access all Reader_Condition;
   --  Handle to Reader_Condition type.

   type Reader_Condition_Set is tagged private;
   --  Set of reader status types

   subtype IO_Request is Thin.SCARD_IO_REQUEST;
   --  Thick binding IO request subtype, used in Transmit() procedure

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

   procedure Add
     (States : in out Card_States_Set;
      State  :        Card_State);
   --  Add a new card state to card states set.

   procedure Add
     (States : in out Reader_States_Set;
      State  :        Reader_State);
   --  Add a new reader state to reader states set.

   procedure Add
     (Set    : in out Reader_Condition_Set;
      Status :        Reader_Condition);
   --  Add a new reader to reader condition array. State specifies the assumed
   --  initial state of the reader/card.

   procedure Begin_Transaction (Card : SCard.Card);
   --  This procedure establishes a temporary exclusive access mode for doing
   --  a series of commands or transactions with a SCard.

   procedure Cancel (Context : SCard.Context);
   --  This procedure cancels all pending blocking requests on the
   --  Status_Change() procedure for a given SCard 'Context'.

   procedure Connect
     (Context :        SCard.Context;
      Card    : in out SCard.Card;
      Reader  :        Reader_ID  := Null_Reader_ID;
      Mode    :        SCard.Mode := Share_Shared);
   --  Connect to a SCard identified by Reader (Reader_ID). Handle to connected
   --  SCard will be stored in 'Card' parameter.

   procedure Control
     (Card        :        SCard.Card;
      Code        :        Natural;
      Send_Buffer :        Byte_Set := Null_Byte_Set;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural);
   --  This procedure sends a control command to the reader connected to by
   --  Connect(). It returns the the control response in Recv_Buffer.

   procedure Disconnect
     (Card   : SCard.Card;
      Action : SCard.Action);
   --  This procedure terminates a connection to a card.

   function Empty (Set : Reader_ID_Set) return Boolean;
   --  This function returns True if the reader ID set contains no readers.

   procedure End_Transaction
     (Card   : SCard.Card;
      Action : SCard.Action);
   --  This procedure ends a previously started transaction.

   procedure Establish_Context
     (Context : in out SCard.Context;
      Scope   :        SCard.Scope);
   --  Establish PC/SC-context. Possible Scope values are defined in the
   --  SCard_Scope type.

   function Find
     (Set       : Reader_Condition_Set;
      Reader_ID : SCard.Reader_ID)
      return Boolean;
   --  Search a given Reader_ID in Reader_Condition_Set. If found, True is
   --  returned, False if not found.

   function First_Index (Set : Reader_ID_Set) return Natural;
   --  Returns the first index of the given reader ID set.

   function First_Index (Set : Reader_States_Set) return Natural;
   --  Returns the first index of the given reader states set.

   function First_Index (Set : Reader_Condition_Set) return Natural;
   --  Returns the first index of the given reader condition set.

   function First_Item (Set : Reader_ID_Set) return Reader_ID;
   --  Returns the first reader ID from the given reader ID set.

   function First_Item (Set : Reader_States_Set) return Reader_State;
   --  Returns the first reader state from the given reader states set.

   function Get
     (Set   : Reader_ID_Set;
      Index : Natural)
      return Reader_ID;
   --  Returns the reader ID at given index in the set.

   function Get
     (Set   : Reader_States_Set;
      Index : Natural)
      return Reader_State;
   --  Returns the reader state at given index in the set.

   function Get
     (Set   : Reader_Condition_Set;
      Index : Natural)
      return Reader_Condition;
   --  Returns the reader condition at given index in the set.

   function Get_Active_Proto (Card : SCard.Card) return Proto;
   --  Return protocol in use for a given card.

   procedure Get_Attribute
     (Card        :        SCard.Card;
      Attr        :        Attribute;
      Recv_Buffer : in out Byte_Set);
   --  This procedure gets an attribute from the IFD handler. Call
   --  Init_Attribute_Set prior to this function to allocate the correct byte
   --  set for the call.

   function Get_PCI (Card : SCard.Card) return Thin.SCARD_IO_REQUEST;
   --  Return PCI to use for a given card. If no valid PCI for a proto is
   --  found, a No_PCI_for_Proto exception will be thrown.

   function Get_Return_Code return String;
   --  Return string representation of last stored return code.

   function Init_Attribute_Set
     (Card : SCard.Card;
      Attr : Attribute)
      return Byte_Set;
   --  This function returns an Null_Byte initialized byte set to provide
   --  storage for an attribute 'Attr'. The byte set can then be used as
   --  parameter to the Get_Attribute function to actually retrieve the
   --  attribute data.

   function Is_In
     (States : Reader_States_Set;
      State  : Reader_State)
      return Boolean;
   --  Function returns True if given reader state is found in the reader
   --  states set.

   function Is_In
     (States : Card_States_Set;
      State  : Card_State)
      return Boolean;
   --  Function returns True if given card state is found in the card states
   --  set.

   function Is_Valid (Context : SCard.Context) return Boolean;
   --  Verify that given SCard context is valid.

   function Last_Index (Set : Reader_ID_Set) return Natural;
   --  Returns the last index of a reader ID set.

   function Last_Index (Set : Reader_States_Set) return Natural;
   --  Returns the last index of a reader states set.

   function Last_Index (Set : Reader_Condition_Set) return Natural;
   --  Returns the last index of a reader condition set.

   function Last_Item (Set : Reader_ID_Set) return Reader_ID;
   --  Returns the last reader ID in a reader ID set.

   function Last_Item (Set : Reader_States_Set) return Reader_State;
   --  Returns the last reader state in a reader states set.

   function Length (Set : Reader_Condition_Set) return Natural;
   --  Returns the length of a reader condition set.

   function List_Readers (Context : SCard.Context) return Reader_ID_Set;
   --  Return list of all available readers for given PC/SC context.

   procedure Reconnect
     (Card   : in out SCard.Card;
      Mode   :        SCard.Mode;
      Action :        SCard.Action);
   --  This procedure re-establishes a connection. The Init argument defines
   --  the desired SCard action to perform on the card/reader.

   procedure Release_Context (Context : in out SCard.Context);
   --  Release previously acquired SCard context.

   procedure Remove
     (States : in out Reader_States_Set;
      State  :        Reader_State);
   --  Remove reader state from given reader states set.

   procedure SPE_Exec (Card : in out SCard.Card; Result : in out Byte_Set);
   --  Request the reader connected to by 'Card' to perform a secure PIN entry
   --  operation. If not already done by the client code, this procedure will
   --  use the SPE_Init() procedure to check if the reader supports SPE and to
   --  initialize the verify ctrl code. If the SPE operation is not supported
   --  or does not work as expected, an SCard_Not_Supported exception is
   --  raised. If the SPE operation is successful, the bytes returned from the
   --  reader will be stored in 'Result' byte set.

   procedure SPE_Init (Card : in out SCard.Card; Result : in out Boolean);
   --  Procedure checks if a reader connected by 'Card' handle supports SPE
   --  (secure PIN entry) operation. If it does, 'Result' will be True,
   --  otherwise 'Result' will be False.

   function Size (Atr : SCard.ATR := Null_ATR) return Natural;
   --  Return current size (in bytes) of an ATR as Natural.

   function Size (Atr : SCard.ATR := Null_ATR) return String;
   --  Return current size (in bytes) of an ATR as String.

   procedure Status
     (Card  :     SCard.Card;
      State : out SCard.Card_States_Set;
      Proto : out SCard.Proto;
      Atr   : out SCard.ATR);
   --  This procedure checks the current status of the reader connected to by
   --  'Card'. Current state, protocol and ATR value of inserted card are
   --  returned as out parameters.

   procedure Status_Change
     (Context    :        SCard.Context;
      Timeout    :        Natural := 0;
      Conditions : in out Reader_Condition_Set);
   --  This procedure takes a reader condition set containing reader names
   --  with their assumed state. It then blocks maximum 'Timeout' milliseconds
   --  time for a change in state to occur. If no timeout is given, 0 will be
   --  used, which will block forever. When a status change occurs, the
   --  Conditions object is updated to reflect the change(s). If this procedure
   --  is called with an empty Conditions object, Wait_For_Readers() is used
   --  to wait for a reader to appear, then the procedure returns.

   procedure Wait_For_Readers (Context : SCard.Context);
   --  This procedure calls SCardGetStatusChange for reader detection. If there
   --  is no reader available, the call will block until a reader is connected.
   --  If there are readers present when this function is called, it will
   --  return immediately.

   procedure Transmit
     (Card        :        SCard.Card;
      Send_Buffer :        Byte_Set := Null_Byte_Set;
      Recv_Pci    : in out IO_Request;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural);
   --  Transmit buffer to SCard.

   procedure Transmit
     (Card        :        SCard.Card;
      Send_Buffer :        Byte_Set := Null_Byte_Set;
      Send_Len    :        Natural;
      Recv_Pci    : in out IO_Request;
      Recv_Buffer : in out Byte_Set;
      Recv_Len    : in out Natural);
   --  Transmit Send_Len bytes of buffer to SCard.

   function To_Atr (Bytes : Byte_Set) return ATR;
   --  Create new ATR object from given byte set. If 'Bytes' is too big to be
   --  converted into an ATR type, a 'Bytes_Too_Big' exception will be raised.

   function To_Reader_ID (Name : String) return Reader_ID;
   --  Returns a new reader ID initialized with 'Name' string.

private

   procedure SCard_Exception
     (Code    : Thin.Return_Code;
      Message : String);
   pragma No_Return (SCard_Exception);
   --  Raise SCard exception if something goes wrong.

   Last_Return_Code : Thin.Return_Code;
   --  Holds the return code of the last SCard operation. Get_Return_Code
   --  can be used to obtain this code. Store_Error should be used to save the
   --  return code after a thin binding call.

   procedure Store_Error (Code : Thin.Return_Code);
   --  Saves the last returned status code.

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

   type ATR_Byte_Count is range 0 .. Thin.MAX_ATR_SIZE + 1;
   --  ATR size in bytes.

   type ATR_Data_Type is new Byte_Set (ATR_Index'Range);
   --  ATR data: Defines a constrained Byte_Set to store the actual ATR data.

   type ATR is record
      Data : ATR_Data_Type;
      Size : ATR_Byte_Count;
   end record;

   Null_ATR : constant ATR := ATR'
     (Data => ATR_Data_Type'(others => Thin.Null_Byte),
      Size => 0);

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

   package VORCP renames Vector_Of_Status_Package;
   subtype Vector_Of_Condition_Type is VORCP.Vector;

   type Reader_Condition_Set is tagged record
      Data : Vector_Of_Condition_Type;
   end record;

end PCSC.SCard;
