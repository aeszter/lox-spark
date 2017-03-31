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
   procedure Define_Full_Type (Spec_File, Body_File : IO.File_Type;
                          Base_Name, Class_Name, Field_List : String);
   procedure Define_Subprogram (Spec_File, Body_File : IO.File_Type;
                          Base_Name, Class_Name, Field_List : String);
   procedure Define_Visitor (File : IO.File_Type; Base_Name : String; Types : String_Array);
   function Substring (Input, Separator : String; Item_Number : Positive) return String;

   generic
      with procedure Process_Field (Field_Name, Type_Name : String);
   procedure Iterate_Fields (List : String);

   procedure Define_Ast (Output_Dir, Base_Name : String; Types : String_Array) is
      Spec_Path : constant String := Output_Dir & "/"
                    & Ada.Characters.Handling.To_Lower (Base_Name) & "s.ads";
      Body_Path : constant String := Output_Dir & "/"
                    & Ada.Characters.Handling.To_Lower (Base_Name) & "s.adb";
      Spec_File, Body_File : IO.File_Type;
   begin
      IO.Create (File => Spec_File, Name => Spec_Path);
      IO.Create (File => Body_File, Name => Body_Path);

      IO.Put_Line (Spec_File, "with L_Strings; use L_Strings;");
      IO.Put_Line (Spec_File, "with Tokens; use Tokens;");
      IO.New_Line (Spec_File);
      IO.Put_Line (Body_File, "with Storage;");
      IO.New_Line (Body_File);
      IO.Put_Line (Spec_File, "package " & Base_Name & "s is");
      IO.Put_Line (Body_File, "package body " & Base_Name & "s is");
      IO.Put_Line (Spec_File, "   type " & Base_Name & " is abstract tagged null record;");
      IO.Put_Line (Spec_File, "   type " & Base_Name & "_Handle is new Positive;");
      -- The AST classes.
      for The_Type of Types loop
         declare
            Class_Name : constant String := Substring (The_Type.all, ":", 1);
            Fields     : constant String := Substring (The_Type.all, ":", 2);
         begin
            Define_Type (Spec_File, Body_File, Base_Name, Class_Name, Fields);
         end;
      end loop;
      Define_Visitor (Spec_File, Base_Name, Types);
      for The_Type of Types loop
         declare
            Class_Name : constant String := Substring (The_Type.all, ":", 1);
            Fields     : constant String := Substring (The_Type.all, ":", 2);
         begin
            Define_Subprogram (Spec_File, Body_File, Base_Name, Class_Name, Fields);
         end;
      end loop;
      IO.New_Line (Spec_File);
      -- The base accept() method.
      IO.Put_Line (Spec_File, "--  abstract <R> R accept(Visitor<R> visitor);");
      IO.Put_Line (Spec_File, "   procedure Accept_Visitor (Self : Expr; V : "
                   & "in out Visitors.Visitor'Class) is abstract;");

      IO.Put_Line (Spec_File, "private");
      for The_Type of Types loop
         declare
            Class_Name : constant String := Substring (The_Type.all, ":", 1);
            Fields     : constant String := Substring (The_Type.all, ":", 2);
         begin
            Define_Full_Type (Spec_File, Body_File, Base_Name, Class_Name, Fields);
         end;
      end loop;

      IO.Put_Line (Spec_File, "end " & Base_Name & "s;");
      IO.Close (Spec_File);
      pragma Unreferenced (Spec_File);
      IO.Put_Line (Body_File, "end " & Base_Name & "s;");
      IO.Close (Body_File);
      pragma Unreferenced (Body_File);
   end Define_Ast;

   procedure Define_Full_Type (Spec_File, Body_File : IO.File_Type;
                          Base_Name, Class_Name, Field_List : String) is
      pragma Unreferenced (Body_File);
      use Ada.Strings.Fixed;

      procedure Define_One_Type (Field_Name, Type_Name : String);

      procedure Define_One_Type (Field_Name, Type_Name : String) is
      begin
         IO.Put_Line (Spec_File, "      " & Field_Name & " : " & Type_Name & ";");
      end Define_One_Type;

      procedure Iterate_Types is new Iterate_Fields (Define_One_Type);

   begin
      IO.Put (Spec_File, "   type " & Class_Name & " is new " & Base_Name & " with ");

      if Field_List'Length > 0 then
         IO.Put_Line (Spec_File, "record");
         Iterate_Types (Field_List);
         IO.Put_Line (Spec_File, "   end record;");
      else
         IO.Put_Line (Spec_File, "null record;");
      end if;
   end Define_Full_Type;

   procedure Define_Subprogram (Spec_File, Body_File : IO.File_Type;
                          Base_Name, Class_Name, Field_List : String) is
      pragma Unreferenced (Field_List);
      use Ada.Strings.Fixed;

   begin
      -- Visitor pattern.
      IO.New_Line (Spec_File);
      IO.Put_Line (Spec_File, "--    <R> R accept(Visitor<R> visitor) {");
      IO.Put_Line (Spec_File, "--      return visitor.visit" &
        Class_Name & Base_Name & "(this);");
      IO.Put_Line (Spec_File, "--    }");
      IO.Put_Line (Spec_File, "   procedure Accept_Visitor (Self : "
                   & Class_Name & "; V : in out Visitors.Visitor'Class);");
      IO.New_Line (Body_File);
      IO.Put_Line (Body_File, "   overriding procedure Accept_Visitor (Self : "
                   & Class_Name & "; V : in out Visitors.Visitor'Class) is");
      IO.Put_Line (Body_File, "   begin");
      IO.Put_Line (Body_File, "      V.Visit_" & Class_Name & "_" & Base_Name & " (Self);");
      IO.Put_Line (Body_File, "   end Accept_Visitor;");

      -- Fields.
      IO.New_Line (Spec_File);

      IO.Put_Line (Spec_File, "--  }");
   end Define_Subprogram;

   procedure Define_Type (Spec_File, Body_File : IO.File_Type;
                          Base_Name, Class_Name, Field_List : String) is
      use Ada.Strings.Fixed;

      procedure Define_Accessor (Field_Name, Type_Name : String);

      procedure Define_Accessor (Field_Name, Type_Name : String) is
      begin
         IO.Put_Line (Spec_File, "   function Get_" & Field_Name & " (Self : "
                      & Class_Name & ") return " & Type_Name & ";");
         IO.New_Line (Body_File);
         IO.Put_Line (Body_File, "   function Get_" & Field_Name & " (Self : "
                      & Class_Name & ") return " & Type_Name & " is");
         IO.Put_Line (Body_File, "   begin");
         IO.Put_Line (Body_File, "      return Self." & Field_Name & ";");
         IO.Put_Line (Body_File, "   end Get_" & Field_Name & ";");
      end Define_Accessor;

      procedure Iterate_Accessors is new Iterate_Fields (Define_Accessor);

   begin
      IO.Put_Line (Spec_File, "");
      IO.Put_Line (Spec_File, "--  static class " & Class_Name & " extends " &
                     Base_Name & " {");
      IO.Put_Line (Spec_File, "   type " & Class_Name & " is new " & Base_Name & " with private;");

      IO.Put_Line (Spec_File, "--    }");
      Iterate_Accessors (Field_List);
   end Define_Type;

   procedure Define_Visitor (File : IO.File_Type; Base_Name : String; Types : String_Array) is
   begin
      IO.Put_Line (File, "   package Visitors is");
      IO.Put_Line (File, "      type Visitor is interface;");
      IO.Put_Line (File, "--  interface Visitor<R> {");

      for The_Type of Types loop
         declare
            Type_Name : constant String  := Substring (The_Type.all, ":", 1);
         begin
            IO.Put_Line (File, "      procedure Visit_" & Type_Name & "_" & Base_Name &
                           " (Self : in out Visitor; The_" & Base_Name &
                           " : " & Type_Name & ") is abstract;");
            IO.Put_Line (File, "--    R visit" & Type_Name & Base_Name & "(" &
                  Type_Name & " " & Ada.Characters.Handling.To_Lower (Base_Name) & ");");
         end;
      end loop;

      IO.Put_Line (File, "   end Visitors;");
   end Define_Visitor;

   procedure Iterate_Fields (List : String) is
      use Ada.Strings.Fixed;
      Before, After : Natural := List'First;
   begin
      while After < List'Last loop
         Before := After;
         After := Index (Source  => List,
                         Pattern => ", ",
                         From    => Before + 1);
         if After = 0 then
            After := List'Last + 1;
         end if;
         if Before = List'First then
            Before := List'First - 1;
         else -- at ","
            Before := Before + 1; -- at " "
         end if;
         declare
            Field : constant String := List (Before + 1 .. After - 1);
            Space : constant Natural := Index (Source => Field,
                                     Pattern => " ",
                                     From    => Field'First);
            Type_Name  : constant String := Field (Field'First .. Space - 1);
            Field_Name : constant String := Field (Space + 1 .. Field'Last);
         begin
            Process_Field (Field_Name, Type_Name);
         end;
      end loop;
   end Iterate_Fields;
   function Substring (Input, Separator : String; Item_Number : Positive) return String is
      use Ada.Strings.Fixed;

      Before, After : Natural := Input'First;

   begin
      for I in 1 .. Item_Number loop
         Before := After;
         After := Index (Source  => Input,
                         Pattern => Separator,
                         From    => Before + 1);
         if After = 0 then
            After := Input'Last + 1;
         end if;
      end loop;
      if Before > Input'First then
         return Trim (Input (Before + 1 .. After - 1), Ada.Strings.Both);
      else
         return Trim (Input (Input'First .. After - 1), Ada.Strings.Both);
      end if;
   end Substring;

   Spec_Binary : aliased constant String :=
     "Binary   :Expr_Handle left, Token operator, Expr_Handle right";
   Spec_Grouping : aliased constant String :=
     "Grouping :Expr_Handle expression";
   -- Unlike Bob's original code, Literal types are separate.
   -- This avoids an indefinite type _inside_ Expr.
   -- Expr'Class is handled in package Storage, and accessed via a simple ID.
   -- Implementing this all over just to deal with different literal types is
   -- overkill. Instead, use the existing infrastructure, i.e. create new types
   -- deriving from Expr.
   Spec_Num_Literal : aliased constant String :=
     "Num_Literal  :Integer value";
   Spec_Str_Literal : aliased constant String :=
     "Str_Literal  :L_String value";
   Spec_Unary : aliased constant String :=
     "Unary    :Token operator, Expr_Handle right";

begin
   if Command_Line.Argument_Count /= 1 then
      IO.Put_Line (IO.Standard_Error, "Usage: generate_ast <output directory>");
      Command_Line.Set_Exit_Status (1);
      return;
   end if;

   Define_Ast (Command_Line.Argument (1), "Expr",
               String_Array'(Spec_Binary'Access,
                 Spec_Grouping'Access,
                 Spec_Num_Literal'Access,
                 Spec_Str_Literal'Access,
                 Spec_Unary'Access));

end Generate_Ast;
