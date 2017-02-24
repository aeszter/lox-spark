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

   function Had_Error return Boolean with
     Refined_Post => Had_Error'Result = My_Error
   is
   begin
      return My_Error;
   end Had_Error;

   procedure Report (Line_No : Positive; Where, Message : String) with
     Refined_Post => My_Error
   is
      Line_Header : constant String (1 .. 5) := "[line";
      Line_Number : constant String :=  Integer'Image (Line_No);
      Error_Infix : constant String := "] Error";
      Message_Separator : constant String := ": ";
   begin
      pragma Assume (Line_Number'Length <= 10, "because of the limited range of type Integer");
      if Status (Standard_Error) = Success and then Is_Writable (Standard_Error) then
         IO.Put_Line (Standard_Error,
                      Line_Header & Line_Number & Error_Infix & Where
                      & Message_Separator & Message);
      end if;
      -- why is this here rather than in procedure Error?
      -- it is not part of the report, after all, and we could put the Status/Writable
      -- check inside Error, around the call to Report
      My_Error := True;
   end Report;

end Error_Reporter;
