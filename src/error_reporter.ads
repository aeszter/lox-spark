with SPARK.Text_IO; use SPARK.Text_IO;

package Error_Reporter with SPARK_Mode,
  abstract_state => State,
    Initializes => State
is
   pragma Elaborate_Body;
   procedure Error (Line_No : Positive; Message : String) with
     Global => (in_out => (Standard_Error, State));
   procedure Report (Line_No : Positive; Where, Message : String) with
     Global => (in_out => Standard_Error, Output => State);
   function Had_Error return Boolean;
   procedure Clear_Error;
private
   My_Error : Boolean := False with Part_Of => State;
end Error_Reporter;
