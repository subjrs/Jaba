with SHA.Strings;
with SHA.Process_Data;
with Ada.Characters.Handling;
with Lib.XMPP;
with Config;


pragma Optimize( Time );
package body Lib.Net is

   procedure Connect (S : in out Sock)
   is
      use Gnat.Sockets;
      use Config;
      Address : Sock_Addr_Type;
      Error : exception;
   begin
      Initialize;
      declare begin
         Address.Addr := Addresses (Get_Host_By_Name (Connection_Server), 1);
      exception
         when others =>
            Address.Addr := Inet_Addr (Connection_Server);
      end;
      Address.Port := Port_Type (Port);
      Create_Socket (S.Socket);
      Set_Socket_Option (S.Socket, Socket_Level, (Keep_Alive, True));
      Connect_Socket (S.Socket, Address);
      S.Channel := Stream (S.Socket);
      Write (S,   "<?xml version='1.0'?>"
                & "<stream:stream xmlns:stream='http://etherx.jabber.org/streams' "
                & "xmlns='jabber:client' to='" & Server & "' >");
      declare
         use Lib.XMPP;
         SID : String := Get_Field (Read (S), "id");
      begin
         Write (S,   "<iq type='get' id='auth_1' to='" & Server & "' >"
                   & "<query xmlns='jabber:iq:auth'>"
                   & "<username>" & Username & "</username>"
                   & "</query></iq>");
         declare Str : String := Read (S); begin null; end;
         Write (S,   "<iq type='set' id='auth_2' to='" & Server & "' >"
                   & "<query xmlns='jabber:iq:auth'>"
                   & "<username>" & Username & "</username>"
                   & "<digest>"
                   & Ada.Characters.Handling.To_Lower
                       (String (SHA.Strings.Hex_From_SHA
                               (SHA.Process_Data.Digest_A_String
                               (SID & Password))))
                   & "</digest>"
                   & "<resource>Jaba</resource>"
                   & "</query></iq>");
         declare 
            Ans : String := Ada.Characters.Handling.To_Lower (Get_Field (Read (S), "type"));
         begin
            if Ans = "error" then
               raise Error;
            end if;            
         end;
      end;
   end Connect;
   
   procedure Disconnect (S : in out Sock) is
      use Gnat.Sockets;
   begin
      Free (S.Channel);
      Shutdown_Socket ( S.Socket );
   end;

   procedure Join_Groupchat (S : Sock; Room : String) is
   begin
      Write (S, "<presence to='" & Room & "/" & Config.Nick & "' ><priority>0</priority></presence>");
   end Join_Groupchat;

   function Sea_To_Str (Sea : Ada.Streams.Stream_Element_Array) return String is
      use Ada.Streams;
      Result : String (Integer(Sea'First) .. Integer (Sea'Last));
      for Result'Address use Sea'Address;
   begin
      return Result;
   end Sea_To_Str;


   function Str_To_Sea (Str : String) return Ada.Streams.Stream_Element_Array is
      use Ada.Streams;
      Result : Stream_Element_Array (Stream_Element_Offset (Str'First) ..
                                     Stream_Element_Offset (Str'Last));
      for Result'Address use Str'Address;
   begin
      return Result;
   end Str_To_Sea;


   function Read (S : Sock; Len : Integer := 1024) return String is
      use Ada.Streams;
      use Gnat.Sockets;
      sea : Stream_Element_Array (1 .. Stream_Element_Offset (Len));
      seo : Stream_Element_Offset := 1;
      Req : Gnat.Sockets.Request_Type (Name => N_Bytes_To_Read);
   begin
      Gnat.Sockets.Receive_Socket (S.Socket, sea, seo);
      Gnat.Sockets.Control_Socket (S.Socket, Req);
      if Integer (seo) = Len and then Req.Size > 0 then
         return Sea_To_Str (sea (1 .. seo)) & Read (S);
      else
         return Sea_To_Str (sea (1 .. seo));
      end if;
   exception
      when others =>
         raise Read_Error;
   end Read;


   procedure Write (S : Sock; Data : String) is
      use Ada.Streams;
      seo : Stream_Element_Offset := Stream_Element_Offset (Data'Length);
      sea : Stream_Element_Array := Str_To_Sea (Data);
   begin
      Gnat.Sockets.Send_Socket (S.Socket, sea, seo);
   end Write;

end Lib.Net;