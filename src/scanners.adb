with Ada.Containers; use Ada.Containers;
-- with Ada.Containers.Formal_Hashed_Maps;
with Tokens; use Tokens;
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

   function Is_Alphanumeric (C : Character) return Boolean is
   begin
      if C in 'a' .. 'z' or else C in 'A' .. 'Z' or else C in '0' .. '9' then
         return True;
      else
         return False;
      end if;
   end Is_Alphanumeric;

   function Is_Decimal_Digit (C : Character) return Boolean is
   begin
      if C in '0' .. '9' then
         return True;
      else
         return False;
      end if;
   end Is_Decimal_Digit;

   function Is_Letter (C : Character) return Boolean is
   begin
      if C in 'a' .. 'z' or else C in 'A' .. 'Z' then
         return True;
      else
         return False;
      end if;
   end Is_Letter;

   procedure Scan_Tokens (Source : String; Token_List : in out Tokens.List) is

      Start, Current : Natural;
      Line           : Positive := 1;

      function Is_At_End return Boolean with
        Global => (Input => (Current, Source)),
      Post => (if Is_At_End'Result = False then Current <= Source'Last);
      procedure Scan_Token with
        Global => (Input  => (Source, Start),
                   in_out => (Current, Line, Token_List, SPARK.Text_IO.Standard_Error, Error_Reporter.State)),
        Pre => Source'First <= Start and then Start <= Current and then Current <= Source'Last and then Source'Last < Integer'Last,
      Post => Current >= Current'Old;

      function Is_At_End return Boolean is
      begin
         return Current > Source'Last;
      end Is_At_End;

      procedure Scan_Token is
         procedure Add_Token (Kind : Token_Kind) with
           Global => (input  => (Start, Current, Source, Line),
                      in_out => (Error_Reporter.State, SPARK.Text_IO.Standard_Error, Token_List)),
         Pre => Source'First <= Start and then Start < Current and then Current - 1 <= Source'Last;
         procedure Add_Token (Kind : Token_Kind; Lexeme : String) with
           Global => (input  => (Line),
                      in_out => (Error_Reporter.State, SPARK.Text_IO.Standard_Error, Token_List));
         procedure Advance (C : out Character) with
           Global => (Input => Source, In_Out => Current),
           Pre => Current >= Source'First and then
           Current <= Source'Last and then
           Current < Integer'Last,
           Post => Current = Current'Old + 1;
         procedure Advance_If_Match (Expected : Character; Match : out Boolean) with
           Global => (in_out => Current, Input => Source),
           Pre => Source'First <= Current and then Source'Last < Integer'Last,
           Post => (if Match then
                      (Current - 1 <= Source'Last and then Current = Current'Old + 1)
                        else
                       Current = Current'Old);
         function Peek return Character with
           Global => (Input => (Current, Source)),
           Pre => Current >= Source'First,
         Post => (if Peek'Result /= NUL then Current <= Source'Last);
         function Peek_Next return Character with
         Pre => Current >= Source'First and then Current < Integer'Last;
         procedure Scan_Identifier with
           Pre => Source'First <= Start and then Start < Current and then
           Current - 1 <= Source'Last and then Source'Last < Integer'Last,
         Post => Current >= Current'Old;
         procedure Scan_Number with
           Pre => Source'First <= Start and then Start < Current and then
                  Current - 1 <= Source'Last and then Source'Last < Integer'Last,
         Post => Current >= Current'Old;
         procedure Scan_String with
           Global => (Input  => (Source, Start),
                      in_out => (Current, Line, Token_List, SPARK.Text_IO.Standard_Error, Error_Reporter.State)),
           Pre => Source'First <= Start and then Start < Current and then Source'Last < Integer'Last,
         Post => Current >= Current'Old;

         procedure Add_Token (Kind : Token_Kind) is
         begin
            Add_Token (Kind, Source (Start .. Current - 1));
         end Add_Token;

         procedure Add_Token (Kind : Token_Kind; Lexeme : String) is
            use Tokens.Lists;
         begin
            if Length (Token_List) >= Token_List.Capacity then
               Error_Reporter.Error (Line_No => Line,
                                     Message => "Out of token capacity");
               return;
            end if;
            Append (Container => Token_List,
                    New_Item  => Tokens.New_Token (Kind   => Kind,
                                                   Lexeme => Lexeme,
                                                   Line   => Line));
         end Add_Token;

         procedure Advance (C : out Character) is
         begin
            Current := Current + 1;
            C := Source (Current - 1);
         end Advance;

         procedure Advance_If_Match (Expected : Character; Match : out Boolean) is
         begin
            if Is_At_End then
               Match := False;
            elsif Source (Current) /= Expected then
               Match := False;
            else
               Current := Current + 1;
               Match := True;
            end if;
         end Advance_If_Match;

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
            while Is_Alphanumeric (Peek)
              or else Peek = '_' loop
               pragma Loop_Invariant (Start <= Current);
               pragma Loop_Invariant (Source'First <= Current and then Current <= Source'Last);
               pragma Loop_Invariant (Start = Start'Loop_Entry);
               pragma Loop_Invariant (Current >= Current'Loop_Entry);
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
            Dummy : Character;
         begin
            while Is_Decimal_Digit (Peek) loop
               pragma Loop_Invariant (Start <= Current);
               pragma Loop_Invariant (Source'First <= Current and then Current <= Source'Last);
               pragma Loop_Invariant (Start = Start'Loop_Entry);
               pragma Loop_Invariant (Current >= Current'Loop_Entry);
               Advance (Dummy);
            end loop;
            -- look for a fractional part
            if Peek = '.' and then Is_Decimal_Digit (Peek_Next) then
               -- consume the '.'
               Advance (Dummy);
               while Is_Decimal_Digit (Peek) loop
                  pragma Loop_Invariant (Start <= Current);
                  pragma Loop_Invariant (Source'First <= Current and then Current <= Source'Last);
                  pragma Loop_Invariant (Start = Start'Loop_Entry);
                  pragma Loop_Invariant (Current >= Current'Loop_Entry);
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
               pragma Loop_Invariant (Source'First <= Current and then Current <= Source'Last);
               pragma Loop_Invariant (Current >= Current'Loop_Entry);
               if Peek = LF then
                  if Line < Integer'Last then
                     Line := Line + 1;
                  else
                     Error_Reporter.Error (Line_No => Line,
                            Message => "Too many lines of source code");
                  end if;
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
         Match : Boolean;
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
               Advance_If_Match (Expected => '=',
                                 Match    => Match);
               if Match then
                  Add_Token (T_BANG_EQUAL);
               else
                  Add_Token (T_BANG);
               end if;
            when '=' =>
               Advance_If_Match (Expected => '=',
                                 Match    => Match);
               if Match then
                  Add_Token (T_EQUAL_EQUAL);
               else
                  Add_Token (T_EQUAL);
               end if;
            when '<' =>
               Advance_If_Match (Expected => '=',
                                 Match    => Match);
               if Match then
                  Add_Token (T_LESS_EQUAL);
               else
                  Add_Token (T_LESS);
               end if;
            when '>' =>
               Advance_If_Match (Expected => '=',
                                 Match    => Match);
               if Match then
                  Add_Token (T_GREATER_EQUAL);
               else
                  Add_Token (T_GREATER);
               end if;
            when '/' =>
               Advance_If_Match (Expected => '/',
                                 Match    => Match);
               if Match then
                  while Peek /= LF and then not Is_At_End loop
                     pragma Loop_Invariant (Source'First <= Current and then Current <= Source'Last);
                     pragma Loop_Invariant (Current >= Current'Loop_Entry);
                     Current := Current + 1;
                  end loop;
               else
                  Add_Token (T_SLASH);
               end if;
            when ' ' | CR | HT =>
               null;
            when LF =>
               if Line < Integer'Last then
                  Line := Line + 1;
               else
                  Error_Reporter.Error (Line_No => Line,
                                        Message => "Too many lines of source code");
               end if;
            when '"' =>
               Scan_String;

            when others =>
               if Is_Decimal_Digit (C) then
                  Scan_Number;
               elsif Is_Letter (C) then
                  Scan_Identifier;
               else
                  Error_Reporter.Error (Line, "Unexpected character.");
               end if;
         end case;
      end Scan_Token;
   begin
      Tokens.Lists.Clear (Token_List);
      Current := Source'First;

      while not Is_At_End loop
         Start := Current;
         pragma Loop_Invariant (Source'First <= Start);
         pragma Loop_Invariant (Start <= Current);
         pragma Loop_Invariant (Current <= Source'Last);
               pragma Assert (Source'First <= Start);
               pragma Assert (Current <= Source'Last);
               pragma Assert (Source'Last < Integer'Last);
         Scan_Token;
      end loop;
      if Tokens.Lists.Length (Token_List) >= Token_List.Capacity then
         Error_Reporter.Error (Line_No => Line,
                               Message => "Out of token capacity");
      else
         Tokens.Lists.Append (Container => Token_List,
                              New_Item  => Tokens.New_Token (Kind   => Tokens.T_EOF,
                                                             Lexeme => "",
                                                             Line   => Line));
      end if;
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
