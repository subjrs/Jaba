-----------------------------------------------------------------------
--   Copyright (C) 2010 by Gavrikov Valeriy                          --
--   subjrs@gmail.com                                                --
--                                                                   --
-- This program is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public License as    --
-- published by the Free Software Foundation; either version 2 of    --
-- the License, or (at your option) any later version.               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Config;
with Jabber;
with Lib.Log;
with Ada.Command_Line;
with Ada.Exceptions;

procedure Jaba is

begin

   loop

   if Ada.Command_Line.Argument_Count >= 1 then
      Config.Load_Config (Ada.Command_Line.Argument (1));
   else
      Config.Load_Config ("jaba.conf");
   end if;
   
   Jabber.Start;
   
   Lib.Log.Write ("Stopped. Waiting 10s before reconnect");
   delay 10.0;
   
   end loop;

exception
   when The_Event: others =>
      Lib.Log.Write (Ada.Exceptions.Exception_Information (The_Event));
end Jaba;