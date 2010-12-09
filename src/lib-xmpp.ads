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