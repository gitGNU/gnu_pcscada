--
--  Copyright (c) 2008,
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

with Ahven.Framework;
with Ahven.Text_Runner;

with Tests_Utils;

with PCSC.SCard.Tests;

procedure Runner is

   pragma Linker_Options ("-lahven");
   pragma Linker_Options ("-lpcsclite");

   S : constant Ahven.Framework.Test_Suite_Access :=
     Ahven.Framework.Create_Suite (Suite_Name => "PCSC/Ada Unit Tests");

begin

   --  Utilities tests

   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Tests_Utils.Test);

   --  SCard Ada <--> C conversion tests

   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new PCSC.SCard.Tests.Test);

   Ahven.Text_Runner.Run (Suite => S);
   Ahven.Framework.Release_Suite (T => S);
end Runner;
