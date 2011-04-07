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

package Config is

   procedure Load_Config (File_Name : String);
   
   function Username                 return String;
   pragma Inline (Username);
   
   function Password                 return String;
   pragma Inline (Password);
   
   function Server                   return String;
   pragma Inline (Server);
   
   function Connection_Server        return String;
   pragma Inline (Connection_Server);
   
   function Port                     return Integer;
   pragma Inline (Port);
   
   function Nick                     return String;
   pragma Inline (Nick);
   
   function Version                  return String;
   pragma Inline (Version);
   
   function OS_Ver                   return String;
   pragma Inline (OS_Ver);

   function Time_Zone                return Integer;
   pragma Inline (Time_Zone);
   
   function URL_Log                  return String;
   pragma Inline (URL_Log);

   function Log_Path                 return String;
   pragma Inline (Log_Path);


   function Room_Count               return Positive;
   pragma Inline (Room_Count);
   
   function Room (Number : Positive) return String;
   pragma Inline (Room);

end Config;
