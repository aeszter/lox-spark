package body Tokens with SPARK_Mode is

   function New_Token
     (Kind : Token_Kind;
      Lexeme : String;
      Line : Positive)
      return Token
   is
   begin
      return Token'(Kind   => Kind,
                    Lexeme => L_Strings.To_Bounded_String (Lexeme),
                    Line   => Line);
   end New_Token;

   function To_String (T : Token) return String is
   begin
      return To_String (T.Kind) & L_Strings.To_String (T.Lexeme);
   end To_String;

   function To_String (T : Token_Kind) return String is
      Image : constant String := Token_Kind'Image (T);
      Result : constant String (1 .. Image'Length) := Image;
   begin
      if Result'Length < 100 then
         return Result;
      else
         return Result (Result'First .. Result'First + 99);
      end if;
   end To_String;

end Tokens;
