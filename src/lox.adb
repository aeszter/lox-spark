with Command_Line; use Command_Line;
with SPARK.Text_IO; use SPARK.Text_IO;
with Error_Reporter;
with Scanners;
with Tokens;

procedure Lox with SPARK_Mode is

   procedure Run (Source : String) with
   Pre => Source'First >= 1 and then Source'Last < Integer'Last;
   procedure Run_File (Path : String);
   procedure Run_Prompt;

   procedure Run (Source : String) is
      Token_List : Tokens.Lists.List (100);
      Position : Tokens.Lists.Cursor;
   begin
      Scanners.Scan_Tokens (Source, Token_List);
      Position := Tokens.Lists.First (Token_List);
      while Tokens.Lists.Has_Element (Token_List, Position) and then
        Is_Writable (Standard_Output) and then Status (Standard_Output) = Success loop
         Put_Line (Tokens.To_String (Tokens.Lists.Element (Token_List, Position)));
         Tokens.Lists.Next (Token_List, Position);
      end loop;
   end Run;

   procedure Run_File (Path : String) is
      Source_File : File_Type;
      Source      : String (1 .. 10_240) := (others => ' ');
      Source_Line : String (1 .. 1_024);
      Last        : Natural;
      Position    : Natural := 1;
      Line_No     : Natural := 0;
   begin
      if Is_Open (Source_File) then
         Error_Reporter.Error (Line_No => 1,
                               Message => "Source file already open");
         return;
      end if;
      if not Is_Open (Source_File) then
         Error_Reporter.Error (Line_No => 1,
                               Message => "Could not open source file");
         return;
      end if;
      Open (The_File => Source_File,
               The_Mode => In_File,
               The_Name => Path);
      while not End_Of_File (Source_File) loop
         Get_Line (File => Source_File,
                   Item => Source_Line,
                   Last => Last);
         if Line_No < Integer'Last then
            Line_No := Line_No + 1;
         else
            Error_Reporter.Error (Line_No => Line_No,
                                  Message => "Too many lines of source code");
            return;
         end if;
         if Position <= Source'Last - Last then
            Source (Position .. Position + Last - 1) := Source_Line (1 .. Last);
            Source (Position + Last) := Scanners.LF;
            Position := Position + Last + 1;
         else
            Error_Reporter.Error (Line_No => Line_No,
                                  Message => "Source code too large for buffer");
            return;
         end if;
      end loop;
      Run (Source (Source'First .. Position - 1));
      if Error_Reporter.Had_Error then
         Command_Line.Set_Exit_Status (65);
      end if;
   end Run_File;

   procedure Run_Prompt is
      Source_Line : String (1 .. 1024);
      Last : Natural;
   begin
      loop
         if Status (Standard_Output) /= Success then
            Error_Reporter.Error (Line_No => 1,
                                  Message => "Session ended");
            return;
         end if;
         Put ("> ");
         Get_Line (Item => Source_Line,
                      Last => Last);
         Run (Source_Line (Source_Line'First .. Last));
         Error_Reporter.Clear_Error;
      end loop;
   end Run_Prompt;

begin
   if Argument_Count > 1 then
      if Status (Standard_Output) = Success then
         Put_Line ("Usage: lox [script]");
      end if;
   elsif Argument_Count = 1 then
      Run_File (Argument (1));
   else
      Run_Prompt;
   end if;
end Lox;
