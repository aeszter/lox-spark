with Ada.Characters.Latin_1;
with SPARK.Text_IO;
with Error_Reporter;
with Tokens;

package Scanners with SPARK_Mode is
   procedure Scan_Tokens (Source : String; Token_List : in out Tokens.List) with
     Global => (in_out => (Error_Reporter.State, SPARK.Text_IO.Standard_Error)),
     Pre => Source'First >= 1 and then Source'Last < Integer'Last;

   LF : constant Character := Ada.Characters.Latin_1.LF;
   NUL : constant Character := Ada.Characters.Latin_1.NUL;
   CR : constant Character := Ada.Characters.Latin_1.CR;
   HT : constant Character := Ada.Characters.Latin_1.HT;

   function Is_Alphanumeric (C : Character) return Boolean with
     Global => null,
     Post => (if Is_Alphanumeric'Result then C /= NUL);
   function Is_Decimal_Digit (C : Character) return Boolean with
     Global => null,
     Post => (if Is_Decimal_Digit'Result then C /= NUL);
   function Is_Letter (C : Character) return Boolean with
     Global => null,
     Post => (if Is_Letter'Result then C /= NUL);
end Scanners;
