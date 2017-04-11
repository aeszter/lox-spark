with Ada.Text_IO;
with Ast_Printers;
with Exprs; use Exprs;
with Storage;
with Tokens; use Tokens;

procedure Test_Ast with SPARK_Mode => Off is
   Test_Expr, U, N, G, F : Expr_Handle;
begin
   Create_Num_Literal (123, N);
   Create_Unary (New_Token (T_MINUS, "-", 1), N, U);
   Create_Float_Literal (45.67, F);
   Create_Grouping (F, G);
   Create_Binary (U, New_Token (T_STAR, "*", 1), G, Test_Expr);
   Ada.Text_IO.Put_Line (Ast_Printers.Print (Storage.Retrieve (Test_Expr)));
end Test_Ast;
