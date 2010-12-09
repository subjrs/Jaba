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