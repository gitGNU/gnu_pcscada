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
--    PC/SC thin-binding package specification. Imports PC/SC functions and
--    provides Ada definitions of types needed to interface with the C API
--    of PC/SC. You should not use this package directly, use the more abstract
--    thick binding provided in the @PCSC.Scard@ package.
--  </PURPOSE>
--

with Interfaces.C;
with Interfaces.C.Strings;

package PCSC.Thin is

   package C renames Interfaces.C;

   --  Type definitions

   subtype LONG  is C.long;
   subtype DWORD is C.unsigned_long;
   subtype LPSTR is C.Strings.chars_ptr;

   --  Byte'n'Hex

   subtype Byte is Interfaces.Unsigned_8;
   type Byte_Array is array (C.size_t range <>) of aliased Byte;

   Null_Byte : constant Byte;
   Null_Byte_Array : constant Byte_Array;


   --  ATR

   MAX_ATR_SIZE : constant := 32;
   --  Maximum ATR size

   subtype ATR is Byte_Array (0 .. MAX_ATR_SIZE);
   --  Binary ATR data

   Null_ATR : constant ATR;
   --  Null initialized ATR


   MAX_BUFFER_SIZE : constant := 264;
   --  Maximum size of a buffer


   type void is null record;
   pragma Convention (C, void);

   type LPVOID  is access void;
   pragma Convention (C, LPVOID);
   type LPCVOID is access constant void;
   pragma Convention (C, LPCVOID);

   subtype SCARDCONTEXT   is LONG;
   --  Context handling related definitions

   subtype SCARDHANDLE    is LONG;
   --  Smartcard handle


   type READERSTATE is record
      szReader       : LPSTR   := C.Strings.Null_Ptr;
      pvUserData     : LPCVOID := null;
      dwCurrentState : DWORD   := 0;
      dwEventState   : DWORD   := 0;
      cbAtr          : DWORD   := MAX_ATR_SIZE;
      rgbAtr         : ATR;
   end record;
   --  Reader state type

   type READERSTATE_Access is access READERSTATE;
   --  Access type to reader state

   type READERSTATE_Array is array (C.size_t range <>) of aliased
     READERSTATE_Access;
   --  Array of access to reader states


   type SCARD_IO_REQUEST is record
      dwProtocol  : DWORD;
      cbPciLength : DWORD;
   end record;
   --  SCard io request structure


   subtype Return_Code is DWORD range 16#0000_0000# .. 16#8010_0069#;
   --  SCard error return codes

   SCARD_S_SUCCESS              : constant := 16#0000_0000#;
   --  No error was encountered

   SCARD_F_INTERNAL_ERROR       : constant := 16#8010_0001#;
   --  An internal consistency check failed

   SCARD_E_CANCELLED            : constant := 16#8010_0002#;
   --  The action was cancelled by an SCardCancel request

   SCARD_E_INVALID_HANDLE       : constant := 16#8010_0003#;
   --  The supplied handle was invalid

   SCARD_E_INVALID_PARAMETER    : constant := 16#8010_0004#;
   --  One or more of the supplied parameters could not be properly
   --  interpreted

   SCARD_E_INVALID_TARGET       : constant := 16#8010_0005#;
   --  Registry startup information is missing or invalid

   SCARD_E_NO_MEMORY            : constant := 16#8010_0006#;
   --  Not enough memory available to complete this command

   SCARD_F_WAITED_TOO_LONG      : constant := 16#8010_0007#;
   --  An internal consistency timer has expired

   SCARD_E_INSUFFICIENT_BUFFER  : constant := 16#8010_0008#;
   --  The data buffer to receive returned data is too small for the returned
   --  data

   SCARD_E_UNKNOWN_READER       : constant := 16#8010_0009#;
   --  The specified reader name is not recognized

   SCARD_E_TIMEOUT              : constant := 16#8010_000A#;
   --  The user-specified timeout value has expired

   SCARD_E_SHARING_VIOLATION    : constant := 16#8010_000B#;
   --  The smart card cannot be accessed because of other connections
   --  outstanding

   SCARD_E_NO_SMARTCARD         : constant := 16#8010_000C#;
   --  The operation requires a Smart Card, but no Smart Card is currently in
   --  the device

   SCARD_E_UNKNOWN_CARD         : constant := 16#8010_000D#;
   --  The specified smart card name is not recognized

   SCARD_E_CANT_DISPOSE         : constant := 16#8010_000E#;
   --  The system could not dispose of the media in the requested manner

   SCARD_E_PROTO_MISMATCH       : constant := 16#8010_000F#;
   --  The requested protocols are incompatible with the protocol currently in
   --  use with the smart card

   SCARD_E_NOT_READY            : constant := 16#8010_0010#;
   --  The reader or smart card is not ready to accept commands

   SCARD_E_INVALID_VALUE        : constant := 16#8010_0011#;
   --  One or more of the supplied parameters values could not be properly
   --  interpreted

   SCARD_E_SYSTEM_CANCELLED     : constant := 16#8010_0012#;
   --  The action was cancelled by the system, presumably to log off or shut
   --  down

   SCARD_F_COMM_ERROR           : constant := 16#8010_0013#;
   --  An internal communications error has been detected

   SCARD_F_UNKNOWN_ERROR        : constant := 16#8010_0014#;
   --  An internal error has been detected, but the source is unknown

   SCARD_E_INVALID_ATR          : constant := 16#8010_0015#;
   --  An ATR obtained from the registry is not a valid ATR string

   SCARD_E_NOT_TRANSACTED       : constant := 16#8010_0016#;
   --  An attempt was made to end a non-existent transaction

   SCARD_E_READER_UNAVAILABLE   : constant := 16#8010_0017#;
   --  The specified reader is not currently available for use

   SCARD_W_UNSUPPORTED_CARD     : constant := 16#8010_0065#;
   --  The reader cannot communicate with the card, due to ATR string
   --  configuration conflicts

   SCARD_W_UNRESPONSIVE_CARD    : constant := 16#8010_0066#;
   --  The smart card is not responding to a reset

   SCARD_W_UNPOWERED_CARD       : constant := 16#8010_0067#;
   --  Power has been removed from the smart card, so that further
   --  communication is not possible

   SCARD_W_RESET_CARD           : constant := 16#8010_0068#;
   --  The smart card has been reset, so any shared state information is
   --  invalid

   SCARD_W_REMOVED_CARD         : constant := 16#8010_0069#;
   --  The smart card has been removed, so further communication is not
   --  possible


   SCARD_E_PCI_TOO_SMALL        : constant := 16#8010_0019#;
   --  The PCI Receive buffer was too small

   SCARD_E_READER_UNSUPPORTED   : constant := 16#8010_001A#;
   --  The reader driver does not meet minimal requirements for support

   SCARD_E_DUPLICATE_READER     : constant := 16#8010_001B#;
   --  The reader driver did not produce a unique reader name

   SCARD_E_CARD_UNSUPPORTED     : constant := 16#8010_001C#;
   --  The smart card does not meet minimal requirements for support

   SCARD_E_NO_SERVICE           : constant := 16#8010_001D#;
   --  The Smart card resource manager is not running

   SCARD_E_SERVICE_STOPPED      : constant := 16#8010_001E#;
   --  The Smart card resource manager has shut down

   SCARD_E_NO_READERS_AVAILABLE : constant := 16#8010_002E#;
   --  Cannot find a smart card reader



   SCARD_SCOPE_USER             : constant := 16#0000#;
   --  Scope in user space

   SCARD_SCOPE_TERMINAL         : constant := 16#0001#;
   --  Scope in terminal

   SCARD_SCOPE_SYSTEM           : constant := 16#0002#;
   --  Scope in system


   SCARD_PROTOCOL_UNDEFINED     : constant := 16#0000#;
   --  protocol not set

   SCARD_PROTOCOL_UNSET         : constant := 16#0000#;
   --  backward compat

   SCARD_PROTOCOL_T0            : constant := 16#0001#;
   --  T=0 active protocol

   SCARD_PROTOCOL_T1            : constant := 16#0002#;
   --  T=1 active protocol

   SCARD_PROTOCOL_RAW           : constant := 16#0004#;
   --  Raw active protocol

   SCARD_PROTOCOL_T15           : constant := 16#0008#;
   --  T=15 protocol

   SCARD_SHARE_EXCLUSIVE        : constant := 16#0001#;
   --  Exclusive mode only

   SCARD_SHARE_SHARED           : constant := 16#0002#;
   --  Shared mode only

   SCARD_SHARE_DIRECT           : constant := 16#0003#;
   --  Raw mode only


   SCARD_LEAVE_CARD             : constant := 16#0000#;
   --  Do nothing on close

   SCARD_RESET_CARD             : constant := 16#0001#;
   --  Reset on close

   SCARD_UNPOWER_CARD           : constant := 16#0002#;
   --  Power down on close

   SCARD_EJECT_CARD             : constant := 16#0003#;
   --  Eject on close


   SCARD_UNKNOWN                : constant := 16#0001#;
   --  Unknown state

   SCARD_ABSENT                 : constant := 16#0002#;
   --  Card is absent

   SCARD_PRESENT                : constant := 16#0004#;
   --  Card is present

   SCARD_SWALLOWED              : constant := 16#0008#;
   --  Card not powered

   SCARD_POWERED                : constant := 16#0010#;
   --  Card is powered

   SCARD_NEGOTIABLE             : constant := 16#0020#;
   --  Ready for PTS

   SCARD_SPECIFIC               : constant := 16#0040#;
   --  PTS has been set


   SCARD_STATE_UNAWARE          : constant := 16#0000#;
   --  App wants status

   SCARD_STATE_IGNORE           : constant := 16#0001#;
   --  Ignore this reader

   SCARD_STATE_CHANGED          : constant := 16#0002#;
   --  State has changed

   SCARD_STATE_UNKNOWN          : constant := 16#0004#;
   --  Reader unknown

   SCARD_STATE_UNAVAILABLE      : constant := 16#0008#;
   --  Status unavailable

   SCARD_STATE_EMPTY            : constant := 16#0010#;
   --  Card removed

   SCARD_STATE_PRESENT          : constant := 16#0020#;
   --  Card inserted

   SCARD_STATE_ATRMATCH         : constant := 16#0040#;
   --  ATR matches card

   SCARD_STATE_EXCLUSIVE        : constant := 16#0080#;
   --  Exclusive Mode

   SCARD_STATE_INUSE            : constant := 16#0100#;
   --  Shared Mode

   SCARD_STATE_MUTE             : constant := 16#0200#;
   --  Unresponsive card

   SCARD_STATE_UNPOWERED        : constant := 16#0400#;
   --  Unpowered card


   SCARD_PCI_T0  : aliased SCARD_IO_REQUEST :=
     (dwProtocol  => SCARD_PROTOCOL_T0, cbPciLength => 8);
   --  Protocol control information (PCI) for T=0

   SCARD_PCI_T1  : aliased SCARD_IO_REQUEST :=
     (dwProtocol  => SCARD_PROTOCOL_T1, cbPciLength => 8);
   --  Protocol control information (PCI) for T=1

   SCARD_PCI_RAW : aliased SCARD_IO_REQUEST :=
     (dwProtocol  => SCARD_PROTOCOL_RAW, cbPciLength => 8);
   --  Protocol control information (PCI) for RAW protocol


   INFINITE                     : constant := 16#FFFF_FFFF#;
   --  Infinite timeout (PC/SC Lite specific extension)


   function SCardEstablishContext
     (dwScope     : in DWORD;
      pvReserved1 : in LPCVOID := null;
      pvReserver2 : in LPCVOID := null;
      phContext   : access SCARDCONTEXT)
      return DWORD;
   --  Establish PC/SC context

   function SCardReleaseContext (hContext : in SCARDCONTEXT) return DWORD;
   --  Release PC/SC context

   function SCardIsValidContext (hContext : in SCARDCONTEXT) return DWORD;
   --  Validate PC/SC context


   function SCardConnect
     (hContext             : in SCARDCONTEXT;
      szReader             : in LPSTR;
      dwShareMode          : in DWORD;
      dwPreferredProtocols : in DWORD;
      phCard               : access SCARDHANDLE;
      pdwActiveProtocol    : access DWORD)
      return DWORD;
   --  Connect to specific SCard

   function SCardReconnect
     (hCard                : in SCARDHANDLE;
      dwShareMode          : in DWORD;
      dwPreferredProtocols : in DWORD;
      dwInitialization     : in DWORD;
      pdwActiveProtocol    : access DWORD)
      return DWORD;
   --  Recconnect to specific SCard

   function SCardDisconnect
     (hCard         : in SCARDHANDLE;
      dwDisposition : in DWORD)
      return DWORD;
   --  Disconnect from specific SCard


   function SCardBeginTransaction (hCard : SCARDHANDLE) return DWORD;
   --  Begin transaction with specific SCard

   function SCardEndTransaction
     (hCard         : SCARDHANDLE;
      dwDisposition : DWORD)
      return DWORD;
   --  End transaction with specific SCard

   function SCardCancelTransaction (hCard : SCARDHANDLE) return DWORD;
   --  Cancel transaction with specific SCard


   function SCardStatus
     (hCard          : in SCARDHANDLE;
      mszReaderNames : in LPSTR;
      pcchReaderLen  : access DWORD;
      pdwState       : access DWORD;
      pdwProtocol    : access DWORD;
      pbAtr          : access Byte;
      pcbAtrLen      : access DWORD)
      return DWORD;
   --  Get status from specific card

   function SCardGetStatusChange
     (hContext       : in SCARDCONTEXT;
      dwTimeout      : in DWORD;
      rgReaderStates : in READERSTATE_Access := null;
      cReaders       : in DWORD := 0)
      return DWORD;
   --  Used to track status changes of readers

   function SCardControl
     (hCard           : in SCARDHANDLE;
      dwControlCode   : in DWORD;
      pbSendBuffer    : access Byte;
      cbSendLength    : in DWORD;
      pbRecvBuffer    : access Byte;
      cbRecvLength    : in DWORD;
      lpBytesReturned : access DWORD)
      return DWORD;
   --  Send control to card

   function SCardTransmit
     (hCard         : in SCARDHANDLE;
      pioSendPci    : access SCARD_IO_REQUEST;
      pbSendBuffer  : access Byte;
      cbSendLength  : in DWORD;
      pioRecvPci    : access SCARD_IO_REQUEST;
      pbRecvBuffer  : access Byte;
      pcbRecvLength : access DWORD)
      return DWORD;
   --  Transmit APDUs to card

   function SCardListReaders
     (hContext    : in SCARDCONTEXT;
      mszGroups   : in LPSTR := C.Strings.Null_Ptr;
      mszReaders  : in LPSTR;
      pcchReaders : access DWORD)
      return DWORD;
   --  List readers

   function SCardListReaderGroups
     (hContext   : in SCARDCONTEXT;
      mszGroups  : in LPSTR;
      pcchGroups : access DWORD)
      return DWORD;
   --  List reader groups

   function SCardGetAttrib
     (hCard      : in SCARDHANDLE;
      dwAttrId   : in DWORD;
      pbAttr     : access Byte;
      pcbAttrLen : access DWORD)
      return DWORD;
   --  Get an attribute from the IFD handler

   function SCardSetAttrib
     (hCard     : in SCARDHANDLE;
      dwAttrId  : in DWORD;
      pbAttr    : access Byte;
      cbAttrLen : in DWORD)
      return DWORD;
   --  Set an attribute of the IFD handler


   function pcsc_stringify_error (status : DWORD) return C.Strings.chars_ptr;
   --  Get stringified error message

private

   Null_ATR : constant ATR := (others => 0);

   Null_Byte : constant Byte := 16#00#;

   Null_Byte_Array : constant Byte_Array (1 .. 0) := (others => Null_Byte);

   --  Imports

   pragma Import (Convention    => C,
                  Entity        => SCardEstablishContext,
                  External_Name => "SCardEstablishContext");
   pragma Import (Convention    => C,
                  Entity        => SCardReleaseContext,
                  External_Name => "SCardReleaseContext");
   pragma Import (Convention    => C,
                  Entity        => SCardIsValidContext,
                  External_Name => "SCardIsValidContext");
   pragma Import (Convention    => C,
                  Entity        => SCardConnect,
                  External_Name => "SCardConnect");
   pragma Import (Convention    => C,
                  Entity        => SCardReconnect,
                  External_Name => "SCardReconnect");
   pragma Import (Convention    => C,
                  Entity        => SCardDisconnect,
                  External_Name => "SCardDisconnect");
   pragma Import (Convention    => C,
                  Entity        => SCardBeginTransaction,
                  External_Name => "SCardBeginTransaction");
   pragma Import (Convention    => C,
                  Entity        => SCardEndTransaction,
                  External_Name => "SCardEndTransaction");
   pragma Import (Convention    => C,
                  Entity        => SCardCancelTransaction,
                  External_Name => "SCardCancelTransaction");
   pragma Import (Convention    => C,
                  Entity        => SCardStatus,
                  External_Name => "SCardStatus");
   pragma Import (Convention    => C,
                  Entity        => SCardGetStatusChange,
                  External_Name => "SCardGetStatusChange");
   pragma Import (Convention    => C,
                  Entity        => SCardControl,
                  External_Name => "SCardControl");
   pragma Import (Convention    => C,
                  Entity        => SCardTransmit,
                  External_Name => "SCardTransmit");
   pragma Import (Convention    => C,
                  Entity        => SCardListReaders,
                  External_Name => "SCardListReaders");
   pragma Import (Convention    => C,
                  Entity        => SCardListReaderGroups,
                  External_Name => "SCardListReaderGroups");
   pragma Import (Convention    => C,
                  Entity        => SCardGetAttrib,
                  External_Name => "SCardGetAttrib");
   pragma Import (Convention    => C,
                  Entity        => SCardSetAttrib,
                  External_Name => "SCardSetAttrib");
   pragma Import (C, pcsc_stringify_error);

end PCSC.Thin;
