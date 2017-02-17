with Ada.Containers.Formal_Doubly_Linked_Lists;
with L_Strings; use L_Strings;

package Tokens with SPARK_Mode is
   type Token_Kind is (
  -- Single-character tokens.
  T_LEFT_PAREN, T_RIGHT_PAREN, T_LEFT_BRACE, T_RIGHT_BRACE,
  T_COMMA, T_DOT, T_MINUS, T_PLUS, T_SEMICOLON, T_SLASH, T_STAR,

  -- One or two character tokens.
  T_BANG, T_BANG_EQUAL,
  T_EQUAL, T_EQUAL_EQUAL,
  T_GREATER, T_GREATER_EQUAL,
  T_LESS, T_LESS_EQUAL,

  -- Literals.
  T_IDENTIFIER, T_STRING, T_NUMBER,

  -- Keywords.
  T_AND, T_CLASS, T_ELSE, T_FALSE, T_FUN, T_FOR, T_IF, T_NIL, T_OR,
  T_PRINT, T_RETURN, T_SUPER, T_THIS, T_TRUE, T_VAR, T_WHILE,

                        T_EOF);

   type Token is record
      Kind : Token_Kind;
      Lexeme : L_String;
      Line   : Positive;
   end record;

   function To_String (T : Token_Kind) return String with
     Global => null,
   Post => To_String'Result'Length <= 100 and then To_String'Result'First = 1;
   function To_String (T : Token) return String with Global => null, Pre => True;
   function New_Token (Kind : Token_Kind; Lexeme : String; Line : Positive) return Token with Global => null, Pre => True;
   package Lists is new Ada.Containers.Formal_Doubly_Linked_Lists (Element_Type => Token);
   subtype List is Lists.List;
   subtype Cursor is  Lists.Cursor;

end Tokens;
