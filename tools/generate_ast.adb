with Ada.Text_IO;
with Command_Line;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Characters;
with Ada.Characters.Handling;

procedure Generate_Ast is
   package IO renames Ada.Text_IO;
   type String_Access is access constant String;
   type String_Array is array (Positive range <>) of String_Access;
   procedure Define_Ast (Output_Dir, Base_Name : String; Types : String_Array);
   procedure Define_Type (File : IO.File_Type; Base_Name,
                           Class_Name, Field_List : String);
   procedure Define_Visitor (File : IO.File_Type; Base_Name : String; Types : String_Array);
   function Substring (Input, Separator : String; Item_Number : Positive) return String;

   procedure Define_Ast (Output_Dir, Base_Name : String; Types : String_Array) is
      Path : constant String := Output_Dir & "/" & Base_Name & ".java";
      File : IO.File_Type;
   begin
      IO.Create (File => File, Name => Path);

      IO.Put_Line (File, "package com.craftinginterpreters.lox;");
      IO.Put_Line (File, "");
      IO.Put_Line (File, "import java.util.List;");
      IO.Put_Line (File, "");
      IO.Put_Line (File, "abstract class " & Base_Name & " {");
      Define_Visitor (File, Base_Name, Types);
      -- The AST classes.
      for The_Type of Types loop
         declare
            Class_Name : constant String := Substring (The_Type.all, ":", 1);
            Fields     : constant String := Substring (The_Type.all, ":", 2);
         begin
            Define_Type (File, Base_Name, Class_Name, Fields);
         end;
      end loop;

      -- The base accept() method.
      IO.Put_Line (File, "");
      IO.Put_Line (File, "  abstract <R> R accept(Visitor<R> visitor);");

      IO.Put_Line (File, "}");
      IO.Close (File);
   end Define_Ast;

   procedure Define_Type (File : IO.File_Type; Base_Name,
                          Class_Name, Field_List : String) is
      use Ada.Strings.Fixed;

      Before, After : Natural := Field_List'First;
   begin
      IO.Put_Line (File, "");
      IO.Put_Line (File, "  static class " & Class_Name & " extends " &
        Base_Name & " {");

      -- Constructor.
      IO.Put_Line (File, "    " & Class_Name & "(" & Field_List & ") {");

      while After < Field_List'Last loop
         Before := After;
         After := Index (Source  => Field_List,
                         Pattern => ", ",
                         From    => Before);
         declare
            Field : constant String := Field_List (Before + 1 .. After - 1);
            Space : constant Natural := Index (Source => Field,
                                     Pattern => " ",
                                     From    => Field'First);
            Name  : constant String := Field (Field'First .. Space - 1);
         begin
            IO.Put_Line (File, "      this." & Name & " = " & Name & ";");
         end;
      end loop;

      IO.Put_Line (File, "    }");
      -- Visitor pattern.
      IO.New_Line (File);
      IO.Put_Line (File, "    <R> R accept(Visitor<R> visitor) {");
      IO.Put_Line (File, "      return visitor.visit" &
        Class_Name & Base_Name & "(this);");
      IO.Put_Line (File, "    }");

      -- Fields.
      IO.New_Line (File);
      After := Field_List'First;
      while After < Field_List'Last loop
         Before := After;
         After := Index (Source  => Field_List,
                         Pattern => ", ",
                         From    => Before);
         declare
            Field : constant String := Field_List (Before + 1 .. After - 1);
         begin
            IO.Put_Line (File, "    final " & Field & ";");
         end;
      end loop;

      IO.Put_Line (File, "  }");
   end Define_Type;

   procedure Define_Visitor (File : IO.File_Type; Base_Name : String; Types : String_Array) is
   begin
      IO.Put_Line (File, "  interface Visitor<R> {");

      for The_Type of Types loop
         declare
            Type_Name : constant String  := Substring (The_Type.all, ":", 1);
         begin
            IO.Put_Line (File, "    R visit" & Type_Name & Base_Name & "(" &
                  Type_Name & " " & Ada.Characters.Handling.To_Lower (Base_Name) & ");");
         end;
      end loop;

      IO.Put_Line (File, "  }");
   end Define_Visitor;

   function Substring (Input, Separator : String; Item_Number : Positive) return String is
      use Ada.Strings.Fixed;

      Before, After : Natural := Input'First;

   begin
      for I in 1 .. Item_Number loop
         Before := After;
         After := Index (Source  => Input,
                          Pattern => Separator,
                         From    => Before);
      end loop;
      if Before > Input'First then
         return Input (Before + 1 .. After - 1);
      else
         return Input (Input'First .. After - 1);
      end if;
   end Substring;

   Spec_Binary : aliased constant String :=
     "Binary   : Expr left, Token operator, Expr right";
   Spec_Grouping : aliased constant String :=
     "Grouping : Expr expression";
   Spec_Literal : aliased constant String :=
     "Literal  : Object value";
   Spec_Unary : aliased constant String :=
     "Unary    : Token operator, Expr right";

begin
   if Command_Line.Argument_Count /= 1 then
      IO.Put_Line (IO.Standard_Error, "Usage: generate_ast <output directory>");
      Command_Line.Set_Exit_Status (1);
      return;
   end if;

   Define_Ast (Command_Line.Argument (1), "Expr",
               String_Array'(Spec_Binary'Access,
                 Spec_Grouping'Access,
                 Spec_Literal'Access,
                 Spec_Unary'Access));

end Generate_Ast;
