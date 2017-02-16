with Ada.Strings.Hash;

package body L_Strings with SPARK_Mode is

   function Hash (S : L_String) return Ada.Containers.Hash_Type is
   begin
      if S.Length = 0 then
         return Ada.Strings.Hash ("");
      else
         return Ada.Strings.Hash (S.Data (1 .. Positive (S.Length)));
      end if;
   end Hash;

   procedure Init (S : out L_String) is
   begin
      S.Data := (others => ' ');
      S.Length := 0;
   end Init;

   function To_Bounded_String
     (Source : String;
      Drop   : Truncation := Right)
      return L_String
   is
      Result : L_String;
      Len    : constant Natural := Source'Length;
   begin
      Init (Result);
      if Len <= Max then
         Result.Data (1 .. Len) := Source;
         Result.Length := Length_T (Len);
      elsif Drop = Left then
         Result.Data := Source (Source'Last - Max + 1 .. Source'Last);
         Result.Length := Length_T (Max);
      elsif Drop = Right then
         Result.Data := Source (Source'First .. Source'First + Max - 1);
         Result.Length := Length_T (Max);
      else
         raise Program_Error;
      end if;
      return Result;
   end To_Bounded_String;

   function To_String (Source : L_String) return String is
   begin
      return Source.Data (1 .. Natural (Source.Length));
   end To_String;

end L_Strings;
