with Exprs; use Exprs;

package Storage is

   function Store (The_Expr : Expr'Class) return Expr_Handle;
   function Retrieve (Handle : Expr_Handle) return Expr'Class;
end Storage;
