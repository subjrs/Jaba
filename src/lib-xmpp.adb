package body Lib.XMPP is

   function Get_Pos (Input, Element  : String) return Integer is
   begin
      return R : Integer := 1 do
         while Input (R .. R + Element'Length - 1) /= Element loop
            R := R + 1;
         end loop;
      end return;
   exception
      when others =>
         return 0;
   end Get_Pos;


   function Get_Body (Input, Name : String;
                  LE : String := "<";
                  RE : String := ">") return String
   is
      I, J : Integer := 1;
   begin
      while Input (I .. I + Name'Length + LE'Length - 1) /= LE & Name loop
         I := I + 1;
      end loop;
      while Input (I .. I + RE'Length - 1) /= RE loop
         I := I + 1;
      end loop;
      I := I + RE'Length;
      J := I;
      while Input (J .. J + Name'Length + LE'Length + RE'Length) /= LE & '/' & Name & RE loop
         J := J + 1;
      end loop;
      J := J - 1;
      return R : String (1 .. J - I + 1) := Input (I .. J);
   exception
      when others =>
         return "";
   end Get_Body;


   function Get_Field (Input, Name : String) return String is
      I, J : Integer := 1;
   begin
      while Input (I .. I + Name'Length) /= Name & '=' loop
         I := I + 1;
      end loop;
      I := I + Name'Length + 2;
      J := I;
      while Input (J .. J) /= "'" and Input (J) /= '"' loop
         J := J + 1;
      end loop;
      J := J - 1;
      return R : String (1 .. J - I + 1) := Input (I .. J);
   exception
      when others =>
         return "";
   end Get_Field;


   function Get_ID (Just_Get : Boolean := False) return String is
   begin
      if not Just_Get then
         ID := ID + 1;
      end if;
      return "Jaba_ID_" & ID_Mod'Image (ID) (2 .. ID'Img'Length);
   end Get_Id;

end Lib.XMPP;