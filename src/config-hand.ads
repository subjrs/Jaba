-- Andry Ogorodnik (andry.ogorodnik@gmail.com)

with Ada.Finalization;

with Ada.Containers.Indefinite_Hashed_Maps;
pragma Elaborate_All (Ada.Containers.Indefinite_Hashed_Maps);

with Ada.Strings.Hash;

private package Config.Hand is

   type Config_Type is
     new Ada.Finalization.Limited_Controlled with private;

   type Config_Type_Access is
     access all Config_Type;

   procedure Open
     (this : in out Config_Type;
      Name :        String);

   procedure Save
     (this : in out Config_Type;
      Name :        String);

   function Get_Option
     (this : Config_Type;
      Name : String)
      return String;

   procedure Set_Option
     (this : in out Config_Type;
      Name :        String;
      Val  :        String);

   procedure Close
     (this : in out Config_Type);

private

   package Options_List is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => String, Element_Type => String,
        Hash => Ada.Strings.Hash, Equivalent_Keys => "=");

   type Config_Type is
     new Ada.Finalization.Limited_Controlled
   with record
      List : Options_List.Map;
   end record;

end Config.Hand;
