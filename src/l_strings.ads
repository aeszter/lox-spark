with Ada; use Ada;
with Ada.Containers;

package L_Strings with SPARK_Mode is

   type L_String is private;
   type Truncation is (Left, Right);

   function Hash (S : L_String) return Ada.Containers.Hash_Type;
--   function "=" (Left, Right : L_String) return Boolean
--                 renames Bounded_Strings."=";
   procedure Init (S : out L_String);
   function To_Bounded_String (Source : String;
                               Drop   : Truncation := Right) return L_String;
   function To_String (Source : L_String) return String;

private
   Max : constant Positive := 255;
   type Length_T is new Integer range 0 .. Max;
   type L_String is record
      Data : String (1 .. Max);
      Length : Length_T;
   end record;

end L_Strings;
