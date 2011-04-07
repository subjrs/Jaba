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

with Config.Hand;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;


package body Config is

   use Ada.Strings.Unbounded;
   
   type Room_Array is array (Positive range <>) of Unbounded_String;
   type Room_Access is access Room_Array;

   type Config_Type is record
      Username          : Unbounded_String;
      Password          : Unbounded_String;
      Server            : Unbounded_String;
      Connection_Server : Unbounded_String;
      Port              : Integer;
      Nick              : Unbounded_String;
      Version           : Unbounded_String;
      OS_Ver            : Unbounded_String;
      Time_Zone		: Integer;
      URL_Log           : Unbounded_String;
      Log_Path          : Unbounded_String;
      Room_Count        : Positive;
      Room              : Room_Access;
   end record;
   
   type Config_Access is access Config_Type;
   pragma Controlled(Config_Access);
   
   Config_Var : Config_Access;
   
   procedure Free (Conf : in out Config_Access) is
      procedure Free_Room is new Ada.Unchecked_Deallocation (Object => Room_Array,
                                                             Name   => Room_Access);
      procedure Free_Conf is new Ada.Unchecked_Deallocation (Object => Config_Type,
                                                             Name   => Config_Access);
   begin
      if Conf /= null then
         if Conf.Room /= null then
            Free_Room (Conf.Room);
         end if;
         Free_Conf (Conf);
      end if;
   end Free;
   
   procedure Load_Config (File_Name : String) is
      CT : Config.Hand.Config_Type;
   begin
      Free (Config_Var);
      Config_Var := new Config_Type;      
      Config.Hand.Open (CT, File_Name);
      Config_Var.Username          := To_Unbounded_String (Config.Hand.Get_Option (CT, "Username"));
      Config_Var.Password          := To_Unbounded_String (Config.Hand.Get_Option (CT, "Password"));
      Config_Var.Server            := To_Unbounded_String (Config.Hand.Get_Option (CT, "Server"));
      Config_Var.Connection_Server := To_Unbounded_String (Config.Hand.Get_Option (CT, "Connection_Server"));
      Config_Var.Port              := Integer'Value       (Config.Hand.Get_Option (CT, "Port"));
      Config_Var.Nick              := To_Unbounded_String (Config.Hand.Get_Option (CT, "Nick"));
      Config_Var.Version           := To_Unbounded_String (Config.Hand.Get_Option (CT, "Version"));
      Config_Var.OS_Ver            := To_Unbounded_String (Config.Hand.Get_Option (CT, "OS_Ver"));
      Config_Var.Time_Zone	   := Integer'Value       (Config.Hand.Get_Option (CT, "Time_Zone"));
      Config_Var.URL_Log           := To_Unbounded_String (Config.Hand.Get_Option (CT, "URL_Log"));
      Config_Var.Log_Path          := To_Unbounded_String (Config.Hand.Get_Option (CT, "Log_Path"));
      Config_Var.Room_Count        := Positive'Value      (Config.Hand.Get_Option (CT, "Room_Count"));
      Config_Var.Room := new Room_Array (1 .. Config_Var.Room_Count);
      for i in Config_Var.Room.all'Range loop
         Config_Var.Room.all (i) := To_Unbounded_String (Config.Hand.Get_Option (CT, "Room" & i'Img (2 .. i'Img'Length)));
      end loop;
      Config.Hand.Close (CT);
   end;   
   
   --------------
   -- Username --
   --------------

   function Username return String is
   begin
      return To_String (Config_Var.Username);
   end Username;

   --------------
   -- Password --
   --------------

   function Password return String is
   begin
      return To_String (Config_Var.Password);
   end Password;

   ------------
   -- Server --
   ------------

   function Server return String is
   begin
      return To_String (Config_Var.Server);
   end Server;

   -----------------------
   -- Connection_Server --
   -----------------------

   function Connection_Server return String is
   begin
      return To_String (Config_Var.Connection_Server);
   end Connection_Server;

   ----------
   -- Port --
   ----------

   function Port return Integer is
   begin
      return Config_Var.Port;
   end Port;

   ----------
   -- Nick --
   ----------

   function Nick return String is
   begin
      return To_String (Config_Var.Nick);
   end Nick;

   -------------
   -- Version --
   -------------

   function Version return String is
   begin
      return To_String (Config_Var.Version);
   end Version;

   ------------
   -- OS_Ver --
   ------------

   function OS_Ver return String is
   begin
      return To_String (Config_Var.OS_Ver);
   end OS_Ver;

   ---------------
   -- Time_Zone --
   ---------------
   
   function Time_Zone return Integer is
   begin
      return Config_Var.Time_Zone;
   end Time_Zone;

   -------------
   -- URL_Log --
   -------------

   function URL_Log return String is
   begin
      return To_String (Config_Var.URL_Log);
   end URL_Log;

   --------------
   -- Log_Path --
   --------------

   function Log_Path return String is
   begin
      return To_String (Config_Var.Log_Path);
   end Log_Path;


   ----------------
   -- Room_Count --
   ----------------

   function Room_Count return Positive is
   begin
      return Config_Var.Room_Count;
   end Room_Count;

   ----------
   -- Room --
   ----------

   function Room (Number : Positive) return String is
   begin
      return To_String (Config_Var.Room (Number));
   end Room;

end Config;


