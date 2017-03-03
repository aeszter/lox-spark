package Command_Line with SPARK_Mode is
   type Exit_Status is new Integer;

   function Argument_Count return Natural with
     Global => null;
   procedure Set_Exit_Status (Code : Exit_Status) with
     Global => null;
   function Argument (Number : Positive) return String with
     Global => null,
   Pre => Number >= 1 and then Number <= Argument_Count;

end Command_Line;
