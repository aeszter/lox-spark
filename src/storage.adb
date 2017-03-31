with Ada.Containers.Formal_Indefinite_Vectors;

package body Storage is

   package Back_Ends is new Ada.Containers.Formal_Indefinite_Vectors
     (Index_Type                   => Expr_Handle,
      Element_Type                 => Expr'Class,
      Max_Size_In_Storage_Elements => Binary'Size,
      Bounded                      => False);

   Container : Back_Ends.Vector := Back_Ends.Empty_Vector;

   function Retrieve (Handle : Expr_Handle) return Expr'Class is
   begin
      return Back_Ends.Element (Container, Handle);
   end Retrieve;

   function Store (The_Expr : Expr'Class) return Expr_Handle is
   begin
      Back_Ends.Append (Container, The_Expr);
      return Back_Ends.Last_Index (Container);
   end Store;

end Storage;
