separate (Exprs)

procedure Store (The_Expr : Expr'Class;
                 Result   : out Expr_Handle;
                 Success  : out Boolean) is
   use Ada.Containers;
   use Storage;
begin
   if Length (Container) >= Container.Capacity then
      Success := False;
      Result := 1;
      return;
   end if;
   Append (Container, The_Expr);
   Result := Last_Index (Container);
   Success := Is_Valid (Result);
end Store;
