package body Error_Reporter with SPARK_Mode,
  Refined_State => (State => My_Error)
is

   package IO renames SPARK.Text_IO;

   procedure Clear_Error is
   begin
      My_Error := False;
   end Clear_Error;

   procedure Error (Line_No : Positive; Message : String) is
   begin
      Report (Line_No => Line_No,
              Where   => "",
              Message => Message);
   end Error;

   function Had_Error return Boolean is
   begin
      return My_Error;
   end Had_Error;

   procedure Report (Line_No : Positive; Where, Message : String) is
   begin
      if Status (Standard_Error) = Success and then Is_Writable (Standard_Error) then
         IO.Put_Line (Standard_Error,
                      "[line" & Integer'Image (Line_No) & "] Error" & Where & ": " & Message);
      end if;
      -- why is this here rather than in procedure Error?
      -- it is not part of the report, after all, and we could put the Status/Writable
      -- check inside Error, around the call to Report
      My_Error := True;
   end Report;

end Error_Reporter;
