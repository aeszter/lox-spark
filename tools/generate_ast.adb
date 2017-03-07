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
   procedure Define_Type (Spec_File, Body_File : IO.File_Type;
                          Base_Name, Class_Name, Field_List : String);
   procedure Define_Visitor (File : IO.File_Type; Base_Name : String; Types : String_Array);
   function Substring (Input, Separator : String; Item_Number : Positive) return String;

   procedure Define_Ast (Output_Dir, Base_Name : String; Types : String_Array) is
      Spec_Path : constant String := Output_Dir & "/" & Base_Name & ".ads";
      Body_Path : constant String := Output_Dir & "/" & Base_Name & ".adb";
      Spec_File, Body_File : IO.File_Type;
   begin
      IO.Create (File => Spec_File, Name => Spec_Path);
      IO.Create (File => Body_File, Name => Body_Path);

      IO.Put_Line (Spec_File, "package " & Base_Name & "s is");
      IO.Put_Line (Body_File, "package body " & Base_Name & "s is");
      IO.Put_Line (Spec_File, "   type " & Base_Name & " is tagged private;");
      Define_Visitor (Spec_File, Base_Name, Types);
      -- The AST classes.
      for The_Type of Types loop
         declare
            Class_Name : constant String := Substring (The_Type.all, ":", 1);
            Fields     : constant String := Substring (The_Type.all, ":", 2);
         begin
            Define_Type (Spec_File, Body_File, Base_Name, Class_Name, Fields);
         end;
      end loop;

      -- The base accept() method.
      IO.Put_Line (Spec_File, "");
      IO.Put_Line (Spec_File, "--  abstract <R> R accept(Visitor<R> visitor);");
      IO.Put_Line (Spec_File, "  function Visit_Expr (Self : Expr; V : Visitor) return R is abstract;");

      IO.Put_Line (Spec_File, "end;");
      IO.Close (Spec_File);
      pragma Unreferenced (Spec_File);
      IO.Put_Line (Body_File, "end;");
      IO.Close (Body_File);
      pragma Unreferenced (Body_File);
   end Define_Ast;

   procedure Define_Type (Spec_File, Body_File : IO.File_Type;
                          Base_Name, Class_Name, Field_List : String) is
      use Ada.Strings.Fixed;

      Before, After : Natural := Field_List'First;
   begin
      IO.Put_Line (Spec_File, "");
      IO.Put_Line (Spec_File, "--  static class " & Class_Name & " extends " &
                     Base_Name & " {");
      IO.Put_Line (Spec_File, "type " & Class_Name & " is new " & Base_Name & ";");
      IO.Put_Line (Spec_File, "function Visit (Self : " & Class_Name &
        "; The_Visitor : Visitor) return R;");

      -- Constructor.
      IO.Put_Line (Spec_File, "--    " & Class_Name & "(" & Field_List & ") {");

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
            IO.Put_Line (Spec_File, "      this." & Name & " = " & Name & ";");
         end;
      end loop;

      IO.Put_Line (Spec_File, "--    }");
      -- Visitor pattern.
      IO.New_Line (Spec_File);
      IO.Put_Line (Spec_File, "--    <R> R accept(Visitor<R> visitor) {");
      IO.Put_Line (Spec_File, "--      return visitor.visit" &
        Class_Name & Base_Name & "(this);");
      IO.Put_Line (Spec_File, "--    }");
      IO.Put_Line (Spec_File, " generic");
      IO.Put_Line (Spec_File, "   type R;");
      IO.Put_Line (Spec_File, " function Visit (Self : " & Class_Name & "; V : Visitor) return R;");
      IO.Put_Line (Body_File, " generic");
      IO.Put_Line (Body_File, "   type R;");
      IO.Put_Line (Body_File, " function Visit (Self : " & Class_Name & "; V : Visitor) return R is");
      IO.Put_Line (Body_File, " begin");
      IO.Put_Line (Body_File, "    return V.Visit" & Class_Name & Base_Name & " (Self);");
      IO.Put_Line (Body_File, " end Visit;");

      -- Fields.
      IO.New_Line (Spec_File);
      After := Field_List'First;
      while After < Field_List'Last loop
         Before := After;
         After := Index (Source  => Field_List,
                         Pattern => ", ",
                         From    => Before);
         declare
            Field : constant String := Field_List (Before + 1 .. After - 1);
         begin
            IO.Put_Line (Spec_File, "    final " & Field & ";");
         end;
      end loop;

      IO.Put_Line (Spec_File, "--  }");
   end Define_Type;

   procedure Define_Visitor (File : IO.File_Type; Base_Name : String; Types : String_Array) is
   begin
      IO.Put_Line (File, "   generic");
      IO.Put_Line (File, "      type R;");
      IO.Put_Line (File, "   package Visitors is");
      IO.Put_Line (File, "      type Visitor is interface;");
      IO.Put_Line (File, "--  interface Visitor<R> {");

      for The_Type of Types loop
         declare
            Type_Name : constant String  := Substring (The_Type.all, ":", 1);
         begin
            IO.Put_Line (File, "   function Visit_" & Type_Name & "_" & Base_Name &
                           " (Self : Visitor; The_" & Base_Name &
                           " : " & Type_Name & ") return R is abstract;");
            IO.Put_Line (File, "--    R visit" & Type_Name & Base_Name & "(" &
                  Type_Name & " " & Ada.Characters.Handling.To_Lower (Base_Name) & ");");
         end;
      end loop;

      IO.Put_Line (File, "end Visitors;");
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
         return Trim (Input (Before + 1 .. After - 1), Ada.Strings.Both);
      else
         return Trim (Input (Input'First .. After - 1), Ada.Strings.Both);
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
