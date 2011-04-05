-----------------------------------------------------------------------
--   Copyright (C) 2011 by Gavrikov Valeriy                          --
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

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Config;
with Lib.XMPP;

package body Lib.Log is


   package ASU  renames Ada.Strings.Unbounded;

   procedure Write (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Get_Time & " " & Message);
   end Write;

   Max : Integer := 1024;

   type Buf_Rec is record
      Conf    : ASU.Unbounded_String;
      From    : ASU.Unbounded_String;
      Message : ASU.Unbounded_String;
      Time    : Ada.Calendar.Time;
   end record;

   type Buf_Array is array (1 .. Max) of Buf_Rec;

   protected type Buffer_Type is
      entry Put (Data : in  Buf_Rec);
      entry Get (Data : out Buf_Rec);
   private
      Buf : Buf_Array;
      Count_In, Count_Out : Integer range 1 .. Max := 1;
      Count : Integer range 0 .. Max := 0;
   end Buffer_Type;
  
   protected body Buffer_Type is

      entry Put (Data : in  Buf_Rec) when Count < Max is
      begin
         Buf (Count_In) := Data;
         Count_In := Count_In mod Max + 1;
         Count := Count + 1;
      end Put;

      entry Get (Data : out Buf_Rec) when Count > 0 is
      begin
         Data := Buf (Count_Out);
         Count_Out := Count_Out mod Max + 1;
         Count := Count - 1;
      end Get;
   end Buffer_Type;

   Buffer : Buffer_Type;


   task Flush;

   task body Flush is
      Data : Buf_Rec;
   begin
      loop
         Buffer.Get (Data);
         declare
            ymdhms  : String                                 := Ada.Calendar.Formatting.Image  (Data.Time, Time_Zone => Ada.Calendar.Time_Zones.Time_Offset (Config.Time_Zone));
            Year    : Ada.Calendar.Year_Number               := Ada.Calendar.Formatting.Year   (Data.Time, Ada.Calendar.Time_Zones.Time_Offset (Config.Time_Zone));
            Month   : Ada.Calendar.Month_Number              := Ada.Calendar.Formatting.Month  (Data.Time, Ada.Calendar.Time_Zones.Time_Offset (Config.Time_Zone));
            Day     : Ada.Calendar.Day_Number                := Ada.Calendar.Formatting.Day    (Data.Time, Ada.Calendar.Time_Zones.Time_Offset (Config.Time_Zone));
            Hour    : Ada.Calendar.Formatting.Hour_Number    := Ada.Calendar.Formatting.Hour   (Data.Time, Ada.Calendar.Time_Zones.Time_Offset (Config.Time_Zone));
            Minute  : Ada.Calendar.Formatting.Minute_Number  := Ada.Calendar.Formatting.Minute (Data.Time, Ada.Calendar.Time_Zones.Time_Offset (Config.Time_Zone));
            Second  : Ada.Calendar.Formatting.Second_Number  := Ada.Calendar.Formatting.Second (Data.Time);
            Conf    : String := ASU.To_String (Data.Conf);
            From    : String := ASU.To_String (Data.From);
            Message : String := ASU.To_String (Data.Message);
            PathD   : String := "log" & Sep & Conf & Sep & Year'Img (2 .. Year'Img'Length) & Sep & Month'Img (2 .. Month'Img'Length);
            PathF   : String := PathD & Sep & Day'Img (2 .. Day'Img'Length) & ".html";

            use Ada.Text_IO;
            FD : File_type;
         begin
            if not Ada.Directories.Exists (PathD) then
               Ada.Directories.Create_Path (PathD);
            end if;
            if not Ada.Directories.Exists (PathF) then
               Create (FD, Out_File, PathF);
               Put_Line (FD, "<html><head><META HTTP-EQUIV=""Content-Type"" CONTENT=""text/html; charset=UTF8""><title>" & PathF & "</title></head>");
               Put_Line (FD, "<body>");
               Close (FD);
            end if;
            declare
               use type ASU.Unbounded_String;
               Parsed : ASU.Unbounded_String;
               j : Integer;
            begin
               j := Message'First;
               loop
                  Parsed := Parsed & Message (j);
                  if Message (j) = ASCII.LF then
                     Parsed := Parsed & "<br>";
                  end if;
                  j := j + 1;
                  exit when j > Message'Last;
               end loop;
               Open (FD, Append_File, PathF);
               Put_Line (FD, "[" & ymdhms (Lib.XMPP.Get_Pos (ymdhms, " ") + 1 .. ymdhms'Length) & "] &lt;" & From & "&gt; " & ASU.To_String (Parsed) & "<br>");
               Close (FD);
            end;
         exception
             when The_Event: others =>
                Write ("Flush Log Task: " & Ada.Exceptions.Exception_Information (The_Event));
                Write ("Flush Log Task: I don't want to die!!!");
         end;
      end loop;

   --  I hope that it never will happen
   exception
      when The_Event: others =>
         Write ("Flush Log Task: " & Ada.Exceptions.Exception_Information (The_Event));
         Write ("Flush Log Task Stopped :( You must restart application to turn on logging.");
   end Flush;


   procedure To_Log (Conf, From, Message : String) is
      Data : Buf_Rec;
   begin
      if Conf = "" then return; end if;
      
      Data.Time    := Ada.Calendar.Clock;
      Data.Conf    := ASU.To_Unbounded_String (Conf);
      Data.From    := ASU.To_Unbounded_String (From);
      Data.Message := ASU.To_Unbounded_String (Message);
      select
         Buffer.Put (Data);
      or
         delay 2.0;
         Write ("Warning: To_Log - Buffer has been busy for 2 seconds!!! Message dropped");
      end select;
   exception
      when The_Event: others =>
         Write ("To_Log: " & Ada.Exceptions.Exception_Information (The_Event));
   end To_Log;

end Lib.Log;
