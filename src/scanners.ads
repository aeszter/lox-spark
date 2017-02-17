with Ada.Characters.Latin_1;
with SPARK.Text_IO;
with Error_Reporter;
with Tokens;

package Scanners with SPARK_Mode is
   procedure Scan_Tokens (Source : String; Token_List : out Tokens.List) with
   Global => (in_out => (Error_Reporter.State, SPARK.Text_IO.Standard_Error));

   LF : constant Character := Ada.Characters.Latin_1.LF;
   NUL : constant Character := Ada.Characters.Latin_1.NUL;
   CR : constant Character := Ada.Characters.Latin_1.CR;
   HT : constant Character := Ada.Characters.Latin_1.HT;
end Scanners;
