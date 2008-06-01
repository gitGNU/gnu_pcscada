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

with Interfaces.C.Strings;
with Ada.Exceptions;

package body PCSC.SCard is

   use IC;
   use IC.Strings;

   C_SCard_Scope : constant array (SCard_Scope) of Thin.DWORD
     := (Scope_User     => Thin.SCARD_SCOPE_USER,
         Scope_Terminal => Thin.SCARD_SCOPE_TERMINAL,
         Scope_System   => Thin.SCARD_SCOPE_SYSTEM);
   --  Map SCard_Scope with the corresponding C values.

   -----------------------
   -- Establish_Context --
   -----------------------

   procedure Establish_Context
     (Context : in out SCard.Context;
      Scope   : in SCard_Scope)
   is
      Res : Thin.DWORD;
   begin
      Res := Thin.SCardEstablishContext
        (dwScope     => C_SCard_Scope (Scope),
         phContext   => Context.C_Context'Access);

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
      Res := Thin.SCardReleaseContext (hContext => Context.C_Context);

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
      Res := Thin.SCardIsValidContext (hContext => Context.C_Context);

      if Res /= Thin.SCARD_S_SUCCESS then
         return False;
      end if;

      return True;
   end Is_Valid;

   ------------------
   -- List_Readers --
   ------------------

   function List_Readers (Context : in SCard.Context) return String is
      Res     : Thin.DWORD;
      Len     : aliased Thin.DWORD;
      Readers : Thin.LPSTR;
   begin
      Res := Thin.SCardListReaders (hContext    => Context.C_Context,
                                    mszReaders  => Null_Ptr,
                                    pcchReaders => Len'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         SCard_Exception (Code    => Res,
                          Message => "Could not get readers");
      end if;

      --  Allocate memory for reader string.
      --  TODO: optimize this -> cannot free this way.
      Readers := IC.Strings.To_Chars_Ptr
        (new IC.char_array (IC.size_t (1) .. IC.size_t (Len)));

      Res := Thin.SCardListReaders (hContext    => Context.C_Context,
                                    mszReaders  => Readers,
                                    pcchReaders => Len'Access);

      if Res /= Thin.SCARD_S_SUCCESS then
         --  TODO: Free Readers here.
         SCard_Exception (Code    => Res,
                          Message => "Could not get readers");
      end if;

      return Value (Readers);

   end List_Readers;

   ---------------------
   -- SCard_Exception --
   ---------------------

   procedure SCard_Exception (Code : in Thin.Return_Code; Message : in String)
   is
      Err_Message : constant String := Value
        (Thin.pcsc_stringify_error (Code));
   begin
      Ada.Exceptions.Raise_Exception
        (SCard_Error'Identity,
         Message & " - ["
           & Thin.DWORD'Image (Code) & "] " & Err_Message);
   end SCard_Exception;

end PCSC.SCard;
