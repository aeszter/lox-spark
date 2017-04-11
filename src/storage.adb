package body Storage with SPARK_Mode,
   Refined_State => (State => Container)
is

   use Back_Ends;

   function Is_Valid (Handle : Expr_Handle) return Boolean with
     Refined_Post => (if Is_Valid'Result then Handle in Back_Ends.First_Index (Container) .. Back_Ends.Last_Index (Container)) is
   begin
      return Handle in First_Index (Container) .. Last_Index (Container);
   end Is_Valid;

   function Retrieve (Handle : Expr_Handle) return Expr'Class is
   begin
      return Back_Ends.Element (Container, Handle);
   end Retrieve;

   procedure Store (The_Expr : Expr'Class;
                    Result   : out Expr_Handle;
                    Success  : out Boolean) is
      use Ada.Containers;
   begin
      if Length (Container) >= Container.Capacity then
         Success := False;
         Result := 1;
         return;
      end if;
      Back_Ends.Append (Container, The_Expr);
      Result := Back_Ends.Last_Index (Container);
      Success := Is_Valid (Result);
   end Store;

end Storage;
