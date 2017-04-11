with Ada.Containers.Formal_Indefinite_Vectors;
with Exprs; use Exprs;

package Storage with SPARK_Mode,
  Abstract_State => State,
    Initializes => State
is
   function Is_Valid (Handle : Expr_Handle) return Boolean;
   procedure Store (The_Expr : Expr'Class;
                    Result   : out Expr_Handle;
                    Success  : out Boolean) with
     Post => (if Success then Is_Valid (Result));
   function Retrieve (Handle : Expr_Handle) return Expr'Class with
     Pre => Is_Valid (Handle);

private
      package Back_Ends is new Ada.Containers.Formal_Indefinite_Vectors
     (Index_Type                   => Expr_Handle,
      Element_Type                 => Expr'Class,
      Max_Size_In_Storage_Elements => Binary'Size);
   -- Should be Bounded => False, but that triggers a prover bug
   pragma Compile_Time_Warning (True, "gnatprove bug workaround");

   Container : Back_Ends.Vector (5) with Part_Of => State;

end Storage;
