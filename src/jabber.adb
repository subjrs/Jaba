with Lib;
with Lib.Log;
with Lib.Net;
with Lib.XMPP;
with Config;
with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Calendar;

package body Jabber is


   Stop_Listener : exception;

   package XMPP renames Lib.XMPP;
   package Net  renames Lib.Net;
   package Log  renames Lib.Log;
   package ASU  renames Ada.Strings.Unbounded;   
   
   Uptime : Ada.Calendar.Time := Ada.Calendar.Clock;
   
   type Query is (Ping, Version);

   type Query_R is
   record
      Q_Time : Ada.Calendar.Time;
      From,
      To     : ASU.Unbounded_String;
      Q_Type : Query;
      Q_ID   : ASU.Unbounded_String;
   end record;

   Querys : array (1 .. 100) of Query_R;
   Query_C : Positive := 1;

   procedure Parse_Message (S : Net.Sock; Raw : String) is
      Message : String := Raw (Raw'First .. Raw'Last);
      MFrom   : String := XMPP.Get_Field (Message, "from");
      MFrom1  : String := MFrom (1 .. XMPP.Get_Pos (MFrom, "/") - 1);
      MFrom2  : String := MFrom (XMPP.Get_Pos (MFrom, "/") + 1 .. MFrom'Length);
      MType   : String := XMPP.Get_Field (Message, "type");
      MID     : String := XMPP.Get_Field (Message, "id");
      
      procedure Groupchat (B : String) is
         MBody : String := B (B'First .. B'Last);
         pvto  : ASU.Unbounded_String;
      begin
         if MBody = "date" or MBody = "date " then
            Net.Write (S, "<message type='groupchat' id='" & XMPP.Get_ID
                        & "' to='" & MFrom1
                        & "'><body>" & MFrom2
                        & ", " & Lib.Get_Time & " (Time_Offset =>" & Config.Time_Zone'Img & ")"
                        & "</body>"
                        & "</message>");
            return;

         elsif MBody'Length >= 4 and then MBody (1 .. 4) = "ping" then
            if MBody = "ping" or MBody = "ping " then
               pvto := ASU.To_Unbounded_String (MFrom);
            elsif MBody'Length > 5 and then MBody (1 .. 5) = "ping " then
               pvto := ASU.To_Unbounded_String (MFrom1 & "/" & MBody (6 .. MBody'Length));
            else
               return;
            end if;            
            Net.Write (S, "<iq type='get'"
                        & " to='" & ASU.To_String (pvto) & "' id='" & XMPP.Get_ID & "'>"
                        & " <query xmlns='jabber:iq:version'/></iq>");
            Querys (Query_C).Q_Time := Ada.Calendar.Clock;
            Querys (Query_C).From := ASU.To_Unbounded_String (MFrom);
            Querys (Query_C).To := pvto;
            Querys (Query_C).Q_Type := Ping;
            Querys (Query_C).Q_ID := ASU.To_Unbounded_String (XMPP.Get_ID (Just_Get => True));
            if Query_C = Querys'Last then
               Query_C := 1;
            else
               Query_C := Query_C + 1;
            end if;
            return;
            
         elsif MBody'Length >= 7 and then MBody (1 .. 7) = "version" then
            if MBody = "version" or MBody = "version " then
               pvto := ASU.To_Unbounded_String (MFrom);
            elsif MBody'Length > 8 and then MBody (1 .. 8) = "version " then
               pvto := ASU.To_Unbounded_String (MFrom1 & "/" & MBody (9 .. MBody'Length));
            else
               return;
            end if;            
            Net.Write (S, "<iq type='get'"
                        & " to='" & ASU.To_String (pvto) & "' id='" & XMPP.Get_ID & "'>"
                        & " <query xmlns='jabber:iq:version'/></iq>");
            Querys (Query_C).From := ASU.To_Unbounded_String (MFrom);
            Querys (Query_C).To := pvto;
            Querys (Query_C).Q_Type := Version;
            Querys (Query_C).Q_ID := ASU.To_Unbounded_String (XMPP.Get_ID (Just_Get => True));
            if Query_C = Querys'Last then
               Query_C := 1;
            else
               Query_C := Query_C + 1;
            end if;
            return;         
         end if;      
      end Groupchat;
      
      procedure Chat (MBody : String) is
      begin
         if MBody = "help" then
            Net.Write (S, "<message type='chat' id='" & XMPP.Get_ID
                        & "' to='" & MFrom
                        & "'><body>"
                        & "ping, version, date"
                        & "</body>"
                        & "</message>");
         elsif MBody = "restart" then
            raise Stop_Listener;
         else
            Net.Write (S, "<message type='chat' id='" & XMPP.Get_ID
                        & "' to='" & MFrom
                        & "'><body>"
                        & "type: help"
                        & "</body>"
                        & "</message>");
         end if;      
      end Chat;
      
      procedure IQ is
      begin
         if MType = "get" and then XMPP.Get_Field (Message, "query xmlns") = "jabber:iq:version" then
            Net.Write (S, "<iq type='result' to='" & MFrom & "' id='" & MID & "' >"
                   & "<query xmlns='jabber:iq:version'>"
                   & "<name>Jaba</name>"
                   & "<version>" & Config.Version & "</version>"
                   & "<os>" & Config.OS_Ver & "</os>"
                   & "</query></iq>");
            return;
            
         elsif MType = "result" and then XMPP.Get_Field (Message, "query xmlns") = "jabber:iq:version" then
            Result: declare
               use ASU;
               use Ada.Calendar;
               i : Positive := 1;
               delt : Duration;
            begin
               while i <= Querys'Last and then Querys (i).Q_ID /= MID loop
                  i := i + 1;
               end loop;
               if i > Querys'Last then
                  return;
               end if;
               Querys (i).Q_ID := To_Unbounded_String (" ");
               if Querys (i).Q_Type = Ping then
                  delt := Ada.Calendar.Clock - Querys (i).Q_Time;
                  declare
                     QFrom : String := To_String (Querys (i).From);
                     QTo   : String := To_String (Querys (i).To);
                  begin
                     if QFrom /= QTo then
                        Net.Write (S, "<message type='groupchat' id='" & XMPP.Get_ID
                                    & "' to='" & MFrom1
                                    & "' ><body>" & QFrom (XMPP.Get_Pos (QFrom, "/") + 1 .. QFrom'Length)
                                    & ", Pong from "
                                    & QTo (XMPP.Get_Pos (QTo, "/") + 1 .. QTo'Length)
                                    & ": " & Duration'Image (delt) (1 .. delt'Img'Length - 3)
                                    & " s</body>"
                                    & "</message>");
                        return;
                     else
                        Net.Write (S, "<message type='groupchat' id='" & XMPP.Get_ID
                                    & "' to='" & MFrom1
                                    & "' ><body>" & QFrom (XMPP.Get_Pos (QFrom, "/") + 1 .. QFrom'Length)
                                    & ", Pong from you"
                                    & ": " & Duration'Image (delt) (1 .. delt'Img'Length - 3)
                                    & " s</body>"
                                    & "</message>");
                        return;
                     end if;
                  end;

               elsif Querys (i).Q_Type = Version then
                  declare
                     QFrom : String := To_String (Querys (i).From);
                     QTo   : String := To_String (Querys (i).To);
                  begin
                     if QFrom /= QTo then
                        Net.Write (S, "<message type='groupchat' id='" & XMPP.Get_ID
                                    & "' to='" & MFrom1
                                    & "' ><body>" & QFrom (XMPP.Get_Pos (QFrom, "/") + 1 .. QFrom'Length)
                                    & ", "        & QTo   (XMPP.Get_Pos (QTo, "/")   + 1 .. QTo'Length)
                                    & " have "    & XMPP.Get_Body (Message, "name") & " "
                                    & XMPP.Get_Body (Message, "version") & " on "
                                    & XMPP.Get_Body (Message, "os")
                                    & "</body>"
                                    & "</message>");
                        return;
                     else
                        Net.Write (S, "<message type='groupchat' id='" & XMPP.Get_ID
                                    & "' to='" & MFrom1
                                    & "' ><body>" & QFrom (XMPP.Get_Pos (QFrom, "/") + 1 .. QFrom'Length)
                                    & ", "
                                    & " you have " & XMPP.Get_Body (Message, "name") & " "
                                    & XMPP.Get_Body (Message, "version") & " on "
                                    & XMPP.Get_Body (Message, "os")
                                    & "</body>"
                                    & "</message>");
                        return;
                     end if;
                  end;
               end if;
            end Result;
         else
            null;
         end if;      
      end IQ;
      
   begin
      -- Log.Write (Message);
   
      if Message (1 .. 9) = "<message " then 
         if MType = "groupchat" then
            Groupchat (XMPP.Get_Body (Message, "body"));
         elsif MType = "chat" then
            Chat (XMPP.Get_Body (Message, "body"));
         else
            Log.Write (Message);
         end if;
         return;
         
      elsif Message (1 .. 4) = "<iq " then
         IQ;
      end if;
   end Parse_Message;


   task type Listener_Type is
      entry Start;
      entry Stop;
   end Listener_Type;
   
   task body Listener_Type is
      Sock : Net.Sock;
   begin
      accept Start;
      Net.Connect (Sock);
      for i in 1 .. Config.Room_Count loop
         Net.Join_Groupchat (Sock, Config.Room (i));
      end loop;
      loop 
         declare 
            Raw : String := Net.Read (Sock);
         begin
            -- Lib.Log.Write (Raw);
            
            if Raw'Length < 3 then
               raise Net.Read_Error;
            end if;
            
            if XMPP.Get_Pos (Raw, "jabber:x:delay") = 0 then
               Parse_Message (Sock, Raw);
            end if;
         exception
             when Net.Read_Error =>
                Log.Write ("Raised Read_Error. Shutting down");
                Net.Disconnect (Sock);
                exit;
             when Stop_Listener =>
                Log.Write ("Raised Stop_Listener. Shutting down");
                Net.Disconnect (Sock);
                exit;
             when The_Event: others =>
                Log.Write (Ada.Exceptions.Exception_Information (The_Event));
         end;
      end loop;
      accept Stop;
   end Listener_Type;   

   procedure Start is
      Listener : Listener_Type;
   begin
      Listener.Start;
      Lib.Log.Write ("Started");
      Listener.Stop;
   end Start;
   
end jabber;