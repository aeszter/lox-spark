with Exprs; use Exprs;
use Exprs.Visitors;
with L_Strings; use L_Strings;

package Ast_Printers is
   -- Creates an unambiguous, if ugly, string representation of AST nodes.
   -- SPARK implementation follows Matthew Heaney
   -- http://www.adapower.com/index.php?Command=Class&ClassID=Patterns&CID=288
   -- This is because in Ada, generic functions cannot be overloaded, so we
   -- cannot follow Bob's implementation
   type Ast_Printer is new Visitor with
      record
         Image : L_String;
      end record;

   function Print (V : Ast_Printer) return String;
   function Print (The_Expr : Expr'Class) return String;

   overriding
   procedure visit_Binary_Expr (Self : in out Ast_Printer; The_Expr : Binary);

   overriding
   procedure visit_Grouping_Expr (Self : in out Ast_Printer; The_Expr : Grouping);

   overriding
   procedure visit_Float_Literal_Expr (Self : in out Ast_Printer; The_Expr : Float_Literal);

   overriding
   procedure visit_Num_Literal_Expr (Self : in out Ast_Printer; The_Expr : Num_Literal);

   overriding
   procedure visit_Str_Literal_Expr (Self : in out Ast_Printer; The_Expr : Str_Literal);

   overriding
   procedure visit_Unary_Expr (Self : in out Ast_Printer; The_Expr : Unary);

private
   function Print (The_Expr : Binary) return String;
   function Print (The_Expr : Grouping) return String;
   function Print (The_Expr : Float_Literal) return String;
   function Print (The_Expr : Num_Literal) return String;
   function Print (The_Expr : Str_Literal) return String;
   function Print (The_Expr : Unary) return String;

end Ast_Printers;
