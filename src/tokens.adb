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
      return Token_Kind'Image (T.Kind) & L_Strings.To_String (T.Lexeme);
   end To_String;

end Tokens;
