with SPARK.Text_IO; use SPARK.Text_IO;

package Error_Reporter with SPARK_Mode,
  Abstract_State => State,
  Initializes => State
is
   pragma Elaborate_Body;
   function Had_Error return Boolean with
   Global => (input => State);
   procedure Error (Line_No : Positive; Message : String) with
     Global => (in_out => (Standard_Error, State)),
     Pre => Message'Length <= 1_024,
     Post => Had_Error;
   procedure Report (Line_No : Positive; Where, Message : String) with
     Global => (in_out => (Standard_Error, State)),
     Pre => Message'Length <= 1_024 and then Where'Length <= 80,
     Post => Had_Error;
   procedure Clear_Error;
private
   My_Error : Boolean := False with Part_Of => State;
end Error_Reporter;
