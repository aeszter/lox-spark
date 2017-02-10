package body Tokens with SPARK_Mode is

   function New_Token
     (Kind : Token_Kind;
      Lexeme : String;
      Line : Positive)
      return Token
   is
   begin
      return Token'(Kind   => Kind,
                    Lexeme => To_Lexeme (Lexeme),
                    Line   => Line);
   end New_Token;

   function To_Lexeme (S : String) return Lexeme_String is
      L : Lexeme_String;
   begin
      L.S (L.S'First .. L.S'First + S'Length - 1) := S;
      L.Length := S'Length;
      return L;
   end To_Lexeme;

   function To_String (T : Token) return String is
   begin
      return T.Kind'Img & To_String (T.Lexeme);
   end To_String;

   function To_String (L : Lexeme_String) return String is
   begin
      return L.S (L.S'First .. L.S'First + L.Length - 1);
   end To_String;

end Tokens;
