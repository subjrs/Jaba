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

with Gnat.Os_Lib;

package Lib is

   procedure Sys (Command : String);

   function Read_File (FN : String) return String;
   pragma Inline (Read_File);
   
   function Get_Time return String;
   pragma Inline (Get_Time);

   Sep : constant Character := Gnat.Os_Lib.Directory_Separator;
   
end Lib;
