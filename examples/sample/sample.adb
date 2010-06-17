with PCSC.SCard;

use PCSC;

procedure Sample is
   pragma Linker_Options ("-lpcsclite");

   Context : SCard.Context;
   --  PC/SC context
   Card    : SCard.Card;
   --  Card handle
   Readers : SCard.Reader_ID_Set;
   --  Set of readers
begin

   --  Establish context

   SCard.Establish_Context (Context => Context,
                            Scope   => SCard.Scope_System);

   --  List readers

   Readers := SCard.List_Readers (Context => Context);

   --  Connect to first reader

   SCard.Connect (Context => Context,
                  Card    => Card,
                  Reader  => Readers.First_Item,
                  Mode    => SCard.Share_Shared);

   --  Send APDU to card

   declare
      Recv_Buffer : SCard.Byte_Set (1 .. 10);
      Send_Buffer : constant SCard.Byte_Set
        := (16#00#, 16#A4#, 16#00#, 16#00#, 16#02#, 16#3F#, 16#00#);
      Recv_Length : Natural := 0;
      Recv_PCI    : SCard.IO_Request;
   begin
      SCard.Transmit (Card        => Card,
                      Send_Buffer => Send_Buffer,
                      Recv_Pci    => Recv_PCI,
                      Recv_Buffer => Recv_Buffer,
                      Recv_Len    => Recv_Length);
   end;

   --  Disconnect

   SCard.Disconnect (Card   => Card,
                     Action => SCard.Reset_Card);

   --  Release context

   SCard.Release_Context (Context => Context);
end Sample;
