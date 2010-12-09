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

with Gnat.Sockets;
with Ada.Streams;

package Lib.Net is

   type Sock is record
      Socket  : GNAT.Sockets.Socket_Type;
      Channel : GNAT.Sockets.Stream_Access;
   end record;

   procedure Connect (S : in out Sock);
                      
   procedure Disconnect (S : in out Sock);

   procedure Join_Groupchat (S : Sock; Room : String);

   function Sea_To_Str (Sea : Ada.Streams.Stream_Element_Array) return String;

   function Str_To_Sea (Str : String) return Ada.Streams.Stream_Element_Array;

   function Read (S : Sock; Len : Integer := 1024) return String;

   Read_Error : Exception;

   procedure Write (S : Sock; Data : String);
   
   
   pragma Inline (Sea_To_Str);
   pragma Inline (Str_To_Sea);
   pragma Inline (Read);
   pragma Inline (Write);

end Lib.Net;