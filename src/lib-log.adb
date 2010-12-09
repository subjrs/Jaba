with Ada.Text_IO;

package body Lib.Log is

   -----------
   -- Write --
   -----------

   procedure Write (Message : String) is
   begin
         Ada.Text_IO.Put_Line (Get_Time & " " & Message);
   end Write;

end Lib.Log;
