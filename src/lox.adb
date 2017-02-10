with Ada.Command_Line; use Ada.Command_Line;
with SPARK.Text_IO;
with Error_Reporter;
with Scanners;
with Tokens;

procedure Lox with SPARK_Mode is
   package IO renames SPARK.Text_IO;

   procedure Run (Source : String);
   procedure Run_File (Path : String);
   procedure Run_Prompt;

   procedure Run (Source : String) is
      Token_List : Tokens.Lists.List (100);
      Position : Tokens.Lists.Cursor;
   begin
      Scanners.Scan_Tokens (Source, Token_List);
      Position := Tokens.Lists.First (Token_List);
      while Tokens.Lists.Has_Element (Token_List, Position) loop
         SPARK.Text_IO.Put_Line (Tokens.To_String (Tokens.Lists.Element (Token_List, Position)));
         Tokens.Lists.Next (Token_List, Position);
      end loop;
   end Run;

   procedure Run_File (Path : String) is
      Source_File : IO.File_Type;
      Source      : String (1 .. 10_240);
      Source_Line : String (1 .. 1_024);
      Last        : Natural;
      Position : Natural := 1;
   begin
      IO.Open (The_File => Source_File,
               The_Mode => IO.In_File,
               The_Name => Path);
      while not IO.End_Of_File (Source_File) loop
         IO.Get_Line (File => Source_File,
                      Item => Source_Line,
                      Last => Last);
         Source (Position .. Position + Last - 1) := Source_Line (1 .. Last);
         Source (Position + Last) := Scanners.LF;
         Position := Position + Last + 1;
      end loop;
      Run (Source (Source'First .. Position - 1));
      if Error_Reporter.Had_Error then
         Ada.Command_Line.Set_Exit_Status (65);
      end if;
   end Run_File;

   procedure Run_Prompt is
      Source_Line : String (1 .. 1024);
      Last : Natural;
   begin
      loop
         IO.Put ("> ");
         IO.Get_Line (Item => Source_Line,
                      Last => Last);
         Run (Source_Line (Source_Line'First .. Last));
         Error_Reporter.Clear_Error;
      end loop;
   end Run_Prompt;

begin
   if Argument_Count > 1 then
      IO.Put_Line ("Usage: lox [script]");
   elsif Argument_Count = 1 then
      Run_File (Argument (1));
   else
      Run_Prompt;
   end if;
end Lox;
