with SPARK.Text_IO;

package body Error_Reporter with SPARK_Mode is

   package IO renames SPARK.Text_IO;

   procedure Clear_Error is
   begin
      My_Error := False;
   end Clear_Error;

   procedure Error (Line : Positive; Message : String) is
   begin
      Report (Line    => Line,
              Where   => "",
              Message => Message);
   end Error;

   function Had_Error return Boolean is
   begin
      return My_Error;
   end Had_Error;

   procedure Report (Line : Positive; Where, Message : String) is
   begin
      IO.Put_Line (IO.Standard_Error,
                   "[line" & Line'Img & "] Error" & Where & ": " & Message);
      My_Error := True;
   end Report;

end Error_Reporter;
