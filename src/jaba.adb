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