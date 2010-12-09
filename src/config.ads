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
   
   function Room_Count               return Positive;
   pragma Inline (Room_Count);
   
   function Room (Number : Positive) return String;
   pragma Inline (Room);

end Config;