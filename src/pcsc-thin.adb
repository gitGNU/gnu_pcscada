package body PCSC.Thin is

   --------------------
   -- SCARD_CTL_CODE --
   --------------------

   function SCARD_CTL_CODE (Code : DWORD) return DWORD is
      use type Interfaces.C.unsigned_long;
   begin
      return 16#4200_0000# + Code;
   end SCARD_CTL_CODE;

end PCSC.Thin;
