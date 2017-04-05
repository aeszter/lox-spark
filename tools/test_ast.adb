with Ada.Text_IO;
with Ast_Printers;
with Exprs; use Exprs;
with Storage;
with Tokens; use Tokens;

procedure Test_Ast with SPARK_Mode => Off is
   Test_Expr : Expr_Handle;
begin
   Test_Expr := Create_Binary (Create_Unary (New_Token (T_MINUS, "-", 1),
                                 Create_Num_Literal (123)),
                               New_Token (T_STAR, "*", 1),
                               Create_Grouping (Create_Float_Literal (45.67)));
   Ada.Text_IO.Put_Line (Ast_Printers.Print (Storage.Retrieve (Test_Expr)));
end Test_Ast;
