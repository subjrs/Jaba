-----------------------------------------------------------------------
--   Copyright (C) 2009 by Gavrikov Valeriy                          --
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

with Ada.Exceptions;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Gnat.Os_Lib;
with System;
with Config;


package body Lib is

   procedure Sys (Command : String) is
      function CSys (C : System.Address) return Integer;
      pragma Import (C, CSys, "system");
      r : Integer;
      C : String := Command & ASCII.Nul;
   begin
      r := CSys (C (C'First)'Address);
   end Sys;

   function Read_File (FN : String) return String is
      FD : Gnat.OS_Lib.File_Descriptor;
      RC : Integer;
      use Gnat.Os_Lib;
   begin
      FD := Open_Read (Name  => FN, FMode => Binary);
      return R : String (1 .. Integer (Gnat.Os_Lib.File_Length (FD))) do
         RC := Gnat.Os_Lib.Read (FD, R(R'First)'Address, R'Length);
         Gnat.Os_Lib.Close (FD);
      end return;
      exception
         when Event: others =>
               return Ada.Exceptions.Exception_Information(Event);
   end Read_File;
   
   function Get_Time return String is
      Time : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      return S : String := Ada.Calendar.Formatting.Image (Time, Time_Zone => Ada.Calendar.Time_Zones.Time_Offset (Config.Time_Zone));
   end Get_Time;

end Lib;
