with Ada.Command_Line;
package body Command_Line with SPARK_Mode => Off is

   function Argument (Number : Positive) return String
   is
   begin
      return Ada.Command_Line.Argument (Number);
   end Argument;

   function Argument_Count return Natural
   is
   begin
      return Ada.Command_Line.Argument_Count;
   end Argument_Count;

   procedure Set_Exit_Status (Code : Exit_Status)
   is
   begin
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Code));
   end Set_Exit_Status;

end Command_Line;
