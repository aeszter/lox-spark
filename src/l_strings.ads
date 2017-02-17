--  with Ada; use Ada;
with Ada.Containers;

package L_Strings with SPARK_Mode is

   Max : constant Integer := 255;
   type L_String is private;
   type Truncation is (Left, Right);

   function Hash (S : L_String) return Ada.Containers.Hash_Type with
     Global => null,
     Pre => True;
   function "=" (Left, Right : L_String) return Boolean with
     Global => null;
   function "=" (Left : L_String; Right : String) return Boolean with
     Global => null;

   procedure Init (S : out L_String) with Global => null, Pre => True;
   function To_Bounded_String (Source : String;
                               Drop   : Truncation := Right) return L_String with
     Global => null,
     Pre => True;
   function To_String (Source : L_String) return String with
     Global => null,
       Pre => True,
   Post => To_String'Result'Length <= Max;

private
   type Length_T is new Integer range 0 .. Max;
   type L_String is record
      Data : String (1 .. Max);
      Length : Length_T;
   end record;

end L_Strings;
