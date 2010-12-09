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

package Lib.XMPP is

   function Get_Pos (Input, Element  : String) return Integer;

   function Get_Body (Input, Name : String;
                      LE : String := "<";
                      RE : String := ">") return String;

   function Get_Field (Input, Name : String) return String;

   function Get_ID (Just_Get : Boolean := False) return String;
   
   pragma Inline (Get_Pos);
   pragma Inline (Get_Body);
   pragma Inline (Get_Field);
   pragma Inline (Get_ID);
   
   
private

   type ID_Mod is mod 2 ** 32;
   ID : ID_Mod := 0;

end Lib.XMPP;