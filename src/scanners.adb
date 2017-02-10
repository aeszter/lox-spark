with Tokens; use Tokens;
with Error_Reporter;

package body Scanners is

   procedure Scan_Tokens (Source : String; Token_List : out Tokens.List) is

      Start, Current : Natural := 1;
      Line           : Natural := 1;

      procedure Scan_Token;

      procedure Scan_Token is
         procedure Add_Token (Kind : Token_Kind);
         procedure Add_Token (Kind : Token_Kind; Lexeme : String);
         function Advance return Character;
         function Match (Expected : Character) return Boolean;
         function Peek return Character;

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

         function Advance return Character is
         begin
            Current := Current + 1;
            return Source (Current - 1);
         end Advance;

         function Match (Expected : Character) return Boolean is
         begin
            if Current = Source'Last then
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
            if Current >= Source'Last then
               return NUL;
            else
               return Source (Current);
            end if;
         end Peek;

         C : constant Character := Advance;
      begin
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
                  while Peek /= LF and then Current <= Source'Last loop
                     Current := Current + 1;
                  end loop;
               else
                  Add_Token (T_SLASH);
               end if;
            when ' ' | CR | HT =>
               null;
               when LF => Line := Line + 1;

               when others => Error_Reporter.Error (Line    => Line,
                                               Message => "Unexpected Character");
         end case;
      end Scan_Token;
   begin
      Tokens.Lists.Clear (Token_List);

      while Current <= Source'Last loop
         Start := Current;
         Scan_Token;
      end loop;
      Tokens.Lists.Append (Container => Token_List,
                           New_Item  => Tokens.New_Token (Kind   => Tokens.T_EOF,
                                                          Lexeme => "",
                                                          Line   => Line));
   end Scan_Tokens;

end Scanners;
