with Ada.Characters.Handling;
-- with Ada.Containers; use Ada.Containers;
-- with Ada.Containers.Formal_Hashed_Maps;
with Tokens; use Tokens;
with Error_Reporter;
with L_Strings; use L_Strings;

package body Scanners with SPARK_Mode is

   --   Note: Instantiation Formal_Hashed_Maps triggers a bug in
   --         gnatprove, so we use a simpler and slower method
   --         instead. Focus of this implementation is provability,
   --         not performance (or even elegance).
--
--     package Hashed_Maps is new
--       Ada.Containers.Formal_Hashed_Maps (Key_Type        => L_String,
--                                          Element_Type    => Token_Kind,
--                                          Hash            => Hash,
--                                          Equivalent_Keys => L_Strings."="
--                                         );
--
--     Keywords : Hashed_Maps.Map (Capacity => 20, Modulus => 97);
--
--     procedure Add_Keyword (Word : String; T : Token_Kind) with
--       Global => Keywords,
--       Pre => Hashed_Maps.Length (Keywords) < 20,
--     Post => Hashed_Maps.Length (Keywords) = Hashed_Maps.Length (Keywords)'Old + 1;
--
--     procedure Add_Keyword (Word : String; T : Token_Kind) is
--     begin
--        Hashed_Maps.Insert (Container => Keywords,
--                            Key       => L_Strings.To_Bounded_String (Word),
--                            New_Item  => T);
--     end Add_Keyword;

   procedure Scan_Tokens (Source : String; Token_List : out Tokens.List) is

      Start, Current : Natural := 1;
      Line           : Natural := 1;

      function Is_At_End return Boolean;
      procedure Scan_Token;

      function Is_At_End return Boolean is
      begin
         return Current > Source'Last;
      end Is_At_End;

      procedure Scan_Token is
         procedure Add_Token (Kind : Token_Kind);
         procedure Add_Token (Kind : Token_Kind; Lexeme : String);
         procedure Advance (C : out Character);
         function Match (Expected : Character) return Boolean;
         function Peek return Character;
         function Peek_Next return Character;
         procedure Scan_Identifier;
         procedure Scan_Number;
         procedure Scan_String;

         procedure Add_Token (Kind : Token_Kind) is
         begin
            Add_Token (Kind, Source (Start .. Current - 1));
         end Add_Token;

         procedure Add_Token (Kind : Token_Kind; Lexeme : String) is
         begin
            Tokens.Lists.Append (Container => Token_List,
                              New_Item  => Tokens.New_Token (Kind   => Kind,
                                                            Lexeme => Lexeme,
                                                            Line   => Line));
         end Add_Token;

         procedure Advance (C : out Character) is
         begin
            Current := Current + 1;
            C := Source (Current - 1);
         end Advance;

         function Match (Expected : Character) return Boolean is
         begin
            if Is_At_End then
               return False;
            end if;
            if Source (Current) /= Expected then
               return False;
            end if;
            Current := Current + 1;
            return True;
         end Match;

         function Peek return Character is
         begin
            if Is_At_End then
               return NUL;
            else
               return Source (Current);
            end if;
         end Peek;

         function Peek_Next return Character is
         begin
            if Current + 1 > Source'Last then
               return NUL;
            else
               return Source (Current + 1);
            end if;
         end Peek_Next;

         procedure Scan_Identifier is
--            use Hashed_Maps;
            Dummy : Character;
         begin
            while Ada.Characters.Handling.Is_Alphanumeric (Peek)
               or else Peek = '_' loop
               Advance (Dummy);
            end loop;
            declare
               Text : constant L_String := L_Strings.To_Bounded_String (Source (Start .. Current - 1));
            begin
--               if Contains (Keywords, Text) then
--                  Add_Token (Element (Keywords, Text));
               if Text = "and" then
                  Add_Token (T_AND);
               elsif Text = "class" then
                  Add_Token (T_CLASS);
               elsif Text = "else" then
                  Add_Token (T_ELSE);
               elsif Text = "false" then
                  Add_Token (T_FALSE);
               elsif Text = "for" then
                  Add_Token (T_FOR);
               elsif Text = "fun" then
                  Add_Token (T_FUN);
               elsif Text = "if" then
                  Add_Token (T_IF);
               elsif Text = "nil" then
                  Add_Token (T_NIL);
               elsif Text = "or" then
                  Add_Token (T_OR);
               elsif Text = "print" then
                  Add_Token (T_PRINT);
               elsif Text = "return" then
                  Add_Token (T_RETURN);
               elsif Text = "super" then
                  Add_Token (T_SUPER);
               elsif Text = "this" then
                  Add_Token (T_THIS);
               elsif Text = "true" then
                  Add_Token (T_TRUE);
               elsif Text = "var" then
                  Add_Token (T_VAR);
               elsif Text = "while" then
                  Add_Token (T_WHILE);
               else
                  Add_Token (T_IDENTIFIER);
               end if;
            end;
         end Scan_Identifier;

         procedure Scan_Number is
            use Ada.Characters.Handling;
            Dummy : Character;
         begin
            while Is_Decimal_Digit (Peek) loop
               Advance (Dummy);
            end loop;

            -- look for a fractional part
            if Peek = '.' and then Is_Decimal_Digit (Peek_Next) then
               -- consume the '.'
               Advance (Dummy);
               while Is_Decimal_Digit (Peek) loop
                  Advance (Dummy);
               end loop;
            end if;
            -- Our Add_Token only takes strings, so, like Ivan, leave out
            -- the value for now.
            --            Add_Token (T_NUMBER, Float'Value (Source (Start .. Current)));
            Add_Token (T_NUMBER);
         end Scan_Number;

         procedure Scan_String is
            Dummy : Character;
         begin
            while not Is_At_End and then Peek /= '"'loop
               if Peek = LF then
                  Line := Line + 1;
               end if;
               Advance (Dummy);
            end loop;
            -- unterminated string
            if Is_At_End then
               Error_Reporter.Error (Line, "Unterminated string.");
               return;
            end if;
            -- the closing "
            Advance (Dummy);

            -- trim the surrounding quotes
            Add_Token (T_STRING, Source (Start + 1 .. Current - 1));
         end Scan_String;

         C :  Character;
      begin
         Advance (C);
         case C is
         when '(' => Add_Token (T_LEFT_PAREN);
         when ')' => Add_Token (T_RIGHT_PAREN);
         when '{' => Add_Token (T_LEFT_BRACE);
         when '}' => Add_Token (T_RIGHT_BRACE);
         when ',' => Add_Token (T_COMMA);
            when '.' => Add_Token (T_DOT);
         when '-' => Add_Token (T_MINUS);
         when '+' => Add_Token (T_PLUS);
         when ';' => Add_Token (T_SEMICOLON);
            when '*' => Add_Token (T_STAR);
            when '!' =>
               if Match ('=') then
                  Add_Token (T_BANG_EQUAL);
               else
                  Add_Token (T_BANG);
               end if;
            when '=' =>
               if Match ('=') then
                  Add_Token (T_EQUAL_EQUAL);
               else
                  Add_Token (T_EQUAL);
               end if;
            when '<' =>
               if Match ('=') then
                  Add_Token (T_LESS_EQUAL);
               else
                  Add_Token (T_LESS);
               end if;
            when '>' =>
               if Match ('=') then
                  Add_Token (T_GREATER_EQUAL);
               else
                  Add_Token (T_GREATER);
               end if;
            when '/' =>
               if Match ('/') then
                  while Peek /= LF and then not Is_At_End loop
                     Current := Current + 1;
                  end loop;
               else
                  Add_Token (T_SLASH);
               end if;
            when ' ' | CR | HT =>
               null;
            when LF => Line := Line + 1;
            when '"' =>
               Scan_String;

            when others =>
               if Ada.Characters.Handling.Is_Decimal_Digit (C) then
                  Scan_Number;
               elsif Ada.Characters.Handling.Is_Letter (C) then
                  Scan_Identifier;
               else
                  Error_Reporter.Error (Line, "Unexpected character.");
               end if;
         end case;
      end Scan_Token;
   begin
      Tokens.Lists.Clear (Token_List);

      while not Is_At_End loop
         Start := Current;
         Scan_Token;
      end loop;
      Tokens.Lists.Append (Container => Token_List,
                           New_Item  => Tokens.New_Token (Kind   => Tokens.T_EOF,
                                                          Lexeme => "",
                                                          Line   => Line));
   end Scan_Tokens;

begin
--     Add_Keyword ("and",    T_AND);
--     Add_Keyword ("class",  T_CLASS);
--     Add_Keyword ("else",   T_ELSE);
--     Add_Keyword ("false",  T_FALSE);
--     Add_Keyword ("for",    T_FOR);
--     Add_Keyword ("fun",    T_FUN);
--     Add_Keyword ("if",     T_IF);
--     Add_Keyword ("nil",    T_NIL);
--     Add_Keyword ("or",     T_OR);
--     Add_Keyword ("print",  T_PRINT);
--     Add_Keyword ("return", T_RETURN);
--     Add_Keyword ("super",  T_SUPER);
--     Add_Keyword ("this",   T_THIS);
--     Add_Keyword ("true",   T_TRUE);
--     Add_Keyword ("var",    T_VAR);
--     Add_Keyword ("while",  T_WHILE);
   null;
end Scanners;
