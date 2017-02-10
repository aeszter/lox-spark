package Error_Reporter with SPARK_Mode is
   procedure Error (Line : Positive; Message : String);
   procedure Report (Line : Positive; Where, Message : String);
   function Had_Error return Boolean;
   procedure Clear_Error;
private
   My_Error : Boolean := False;
end Error_Reporter;
