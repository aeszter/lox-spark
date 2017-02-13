with Ada; use Ada;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;

package L_Strings with SPARK_Mode is
   package Bounded_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 255);
   subtype L_String is Bounded_Strings.Bounded_String;

   function Hash is new Ada.Strings.Bounded.Hash (Bounded_Strings);
   function "=" (Left, Right : L_String) return Boolean
                 renames Bounded_Strings."=";
   function To_Bounded_String (Source : String;
                               Drop   : Truncation := Error) return L_String
                               renames Bounded_Strings.To_Bounded_String;
   function To_String (Source : L_String) return String
                       renames Bounded_Strings.To_String;
end L_Strings;
