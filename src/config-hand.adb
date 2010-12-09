-- Andry Ogorodnik (andry.ogorodnik@gmail.com)

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Config.Hand is

   delim : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set ('=');

   end_key : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (';');

   ----------
   -- Open --
   ----------

   procedure Open
     (this : in out Config_Type;
      Name :        String)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;

      f : File_Type;

      delim_pos   : Natural;
      end_key_pos : Natural;
      
      Config_File_Not_Found : Exception;
   begin
      if not Ada.Directories.Exists (Name) then
         raise Config_File_Not_Found with Name;
      --  return;
      end if;

      try : begin
         Open (f, In_File, Name);
      --  exception
      --     when others =>
      --        return;
      end try;

      while not End_Of_File (f) loop
         try1 :
         declare
            s : constant String := Get_Line (f);
         begin
            if s (s'First) /= '#' then
               delim_pos := Index (s, delim);

               if delim_pos > s'First then
                  end_key_pos := Index (s (delim_pos + 1 ..  s'Last), end_key);

                  if end_key_pos > delim_pos then
                     this.List.Insert
                       (Trim (s (s'First .. delim_pos - 1), Ada.Strings.Both),
                        Trim (s (delim_pos + 1 .. end_key_pos - 1),
                          Ada.Strings.Both));
                  else
                     this.List.Insert
                       (Trim (s (s'First .. delim_pos - 1), Ada.Strings.Both),
                        Trim (s (delim_pos + 1 .. s'Last), Ada.Strings.Both));
                  end if;
               end if;
            end if;

         exception
            when others =>
               null;
         end try1;
      end loop;

      Close (f);
   end Open;

   ----------
   -- Save --
   ----------

   procedure Save
     (this : in out Config_Type;
      Name :        String)
   is
      use Ada.Text_IO;
      use Options_List;

      f : File_Type;

      procedure Save_Element (c : Cursor);

      procedure Save_Element (c : Cursor) is
      begin
         Put_Line (f, Key (c) & '=' & Element (c) & ';');
      end Save_Element;
   begin
      try : begin
         Open (f, Out_File, Name);
      exception
         when others =>
            null;
      end try;

      try1 : begin
         if not Is_Open (f) then
            Create (f, Out_File, Name);
         end if;
      exception
         when others =>
            return;
      end try1;

      this.List.Iterate (Save_Element'Access);
      Close (f);
   end Save;

   ----------------
   -- Get_Option --
   ----------------

   function Get_Option
     (this : Config_Type;
      Name : String)
      return String
   is
      use Options_List;

      c : Cursor;
      Key_Not_Found : Exception;
   begin
      c := this.List.Find (Name);

      if c /= No_Element then
         return Element (c);
      else
         raise Key_Not_Found with "Key <" & Name & "> not found";
         -- return "";
      end if;
   end Get_Option;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option
     (this : in out Config_Type;
      Name :        String;
      Val  :        String)
   is
      use Options_List;

      c : Cursor;
   begin
      c := this.List.Find (Name);

      if c /= No_Element then
         this.List.Replace_Element (c, Val);
      else
         this.List.Insert (Name, Val);
      end if;
   end Set_Option;

   -----------
   -- Close --
   -----------

   procedure Close
     (this : in out Config_Type)
   is
   begin
      this.List.Clear;
   end Close;

end Config.Hand;
