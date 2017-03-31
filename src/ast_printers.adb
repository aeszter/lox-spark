with Storage; use Storage;

package body Ast_Printers is

   function Print (The_Expr : Expr'Class) return String is
      V : Ast_Printer;
   begin
      Accept_Visitor (The_Expr, V);
      return Print (V);
   end Print;

   function Print (V : Ast_Printer) return String is
   begin
      return To_String (V.Image);
   end Print;

   function Print (The_Expr : Binary) return String is
   begin
      return "(" & To_String (The_Expr.Get_operator.Lexeme) & " "
        & Print (Retrieve (The_Expr.Get_left)) & " "
        & Print (Retrieve (The_Expr.Get_right)) & ")";
   end Print;

   function Print (The_Expr : Grouping) return String is
   begin
      return "(group " & Print (Retrieve (The_Expr.Get_expression)) & ")";
   end Print;

   function Print (The_Expr : Float_Literal) return String is
   begin
      return Float'Image (The_Expr.Get_value);
   end Print;

   function Print (The_Expr : Num_Literal) return String is
   begin
      return Integer'Image (The_Expr.Get_value);
   end Print;

   function Print (The_Expr : Str_Literal) return String is
   begin
      return L_Strings.To_String (The_Expr.Get_value);
   end Print;

   function Print (The_Expr : Unary) return String is
   begin
      return "(" & To_String (The_Expr.Get_operator.Lexeme) & " " & Print (Retrieve (The_Expr.Get_right)) & ")";
   end Print;

   generic
      type Expr_Type (<>) is abstract new Expr with private;
      with function Print (The_Expr : Expr_Type) return String is <>;
   procedure Visit_Expr (V : in out Ast_Printer; The_Expr : Expr_Type);

   procedure Visit_Expr (V : in out Ast_Printer; The_Expr : Expr_Type) is
      function Local_Print (E : Expr_Type) return String
        renames Print; -- resolve ambiguity
      Image : constant String := Local_Print (The_Expr);
   begin
      V.Image := To_Bounded_String (Image);
   end Visit_Expr;

   procedure Do_Visit_Binary_Expr is new Visit_Expr (Binary);

   overriding
   procedure visit_Binary_Expr (Self : in out Ast_Printer; The_Expr : Binary)
                                renames Do_Visit_Binary_Expr;

   procedure Do_Visit_Grouping_Expr is new Visit_Expr (Grouping);

   overriding
   procedure visit_Grouping_Expr (Self : in out Ast_Printer; The_Expr : Grouping)
                                  renames Do_Visit_Grouping_Expr;

   procedure Do_Visit_Float_Literal_Expr is new Visit_Expr (Float_Literal);

   overriding
   procedure visit_Float_Literal_Expr (Self : in out Ast_Printer; The_Expr : Float_Literal)
                                     renames Do_Visit_Float_Literal_Expr;
   procedure Do_Visit_Num_Literal_Expr is new Visit_Expr (Num_Literal);

   overriding
   procedure visit_Num_Literal_Expr (Self : in out Ast_Printer; The_Expr : Num_Literal)
                                     renames Do_Visit_Num_Literal_Expr;

   procedure Do_Visit_str_literal_Expr is new Visit_Expr (Str_Literal);

   overriding
   procedure visit_Str_Literal_Expr (Self : in out Ast_Printer; The_Expr : Str_Literal)
                                     renames Do_Visit_str_literal_Expr;

   procedure Do_Visit_unary_Expr is new Visit_Expr (Unary);

   overriding
   procedure visit_Unary_Expr (Self : in out Ast_Printer; The_Expr : Unary)
     renames Do_Visit_unary_Expr;

end Ast_Printers;
